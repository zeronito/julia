# This file is a part of Julia. License is MIT: http://julialang.org/license

module Generate

import ...LibGit2, ..Read, ...Pkg.PkgError
importall ...LibGit2

copyright_year() =  string(Dates.year(Dates.today()))
copyright_name(repo::GitRepo) = LibGit2.getconfig(repo, "user.name", "")
github_user() = LibGit2.getconfig("github.user", "")

function git_contributors(repo::GitRepo, n::Int=typemax(Int))
    contrib = Dict()
    for sig in LibGit2.authors(repo)
        if haskey(contrib, sig.email)
            contrib[sig.email][1] += 1
        else
            contrib[sig.email] = [1, sig.name]
        end
    end

    names = Dict()
    for (commits,name) in values(contrib)
        names[name] = get(names,name,0) + commits
    end
    names = sort!(collect(keys(names)),by=name->names[name],rev=true)
    length(names) <= n ? names : [names[1:n]; "et al."]
end

function package(
    pkg::AbstractString,
    license::AbstractString;
    force::Bool = false,
    authors::Union{AbstractString,Array} = "",
    years::Union{Int,AbstractString} = copyright_year(),
    user::AbstractString = github_user(),
    config::Dict = Dict(),
)
    isnew = !ispath(pkg)
    try
        repo = if isnew
            url = isempty(user) ? "" : "https://github.com/$user/$pkg.jl.git"
            Generate.init(pkg,url,config=config)
        else
            repo = GitRepo(pkg)
            if LibGit2.isdirty(repo)
                finalize(repo)
                throw(PkgError("$pkg is dirty – commit or stash your changes"))
            end
            repo
        end

        LibGit2.transact(repo) do repo
            if isempty(authors)
                authors = isnew ? copyright_name(repo) : git_contributors(repo,5)
            end

            files = [Generate.license(pkg,license,years,authors,force=force),
                     Generate.readme(pkg,user,force=force),
                     Generate.entrypoint(pkg,force=force),
                     Generate.tests(pkg,force=force),
                     Generate.require(pkg,force=force),
                     Generate.travis(pkg,force=force),
                     Generate.appveyor(pkg,force=force),
                     Generate.gitignore(pkg,force=force) ]

            msg = """
            $pkg.jl $(isnew ? "generated" : "regenerated") files.

                license:  $license
                authors:  $(join(vcat(authors),", "))
                years:    $years
                user:     $user

            Julia Version $VERSION [$(Base.GIT_VERSION_INFO.commit_short)]
            """
            LibGit2.add!(repo, files..., flags = LibGit2.Consts.INDEX_ADD_FORCE)
            if isnew
                info("Committing $pkg generated files")
                LibGit2.commit(repo, msg)
            elseif LibGit2.isdirty(repo)
                LibGit2.remove!(repo, files...)
                info("Regenerated files left unstaged, use `git add -p` to select")
                open(io->print(io,msg), joinpath(LibGit2.gitdir(repo),"MERGE_MSG"), "w")
            else
                info("Regenerated files are unchanged")
            end
        end
    catch
        isnew && rm(pkg, recursive=true)
        rethrow()
    end
    return
end

function init(pkg::AbstractString, url::AbstractString=""; config::Dict=Dict())
    if !ispath(pkg)
        info("Initializing $pkg repo: $(abspath(pkg))")
        repo = LibGit2.init(pkg)
        try
            with(GitConfig, repo) do cfg
                for (key,val) in config
                    LibGit2.set!(cfg, key, val)
                end
            end
            LibGit2.commit(repo, "initial empty commit")
        catch err
            throw(PkgError("Unable to initialize $pkg package: $err"))
        end
    else
        repo = GitRepo(pkg)
    end
    try
        if !isempty(url)
            info("Origin: $url")
            with(LibGit2.GitRemote, repo, "origin", url) do rmt
                LibGit2.save(rmt)
            end
            LibGit2.set_remote_url(repo, url)
        end
    end
    return repo
end

function genfile(f::Function, pkg::AbstractString, file::AbstractString, force::Bool=false)
    path = joinpath(pkg,file)
    if force || !ispath(path)
        info("Generating $file")
        mkpath(dirname(path))
        open(f, path, "w")
        return file
    end
    return ""
end

function license(pkg::AbstractString,
                 license::AbstractString,
                 years::Union{Int,AbstractString},
                 authors::Union{AbstractString,Array};
                 force::Bool=false)
    file = genfile(pkg,"LICENSE.md",force) do io
        if !haskey(LICENSES,license)
            licenses = join(sort!(collect(keys(LICENSES)), by=lowercase), ", ")
            throw(PkgError("$license is not a known license choice, choose one of: $licenses."))
        end
        print(io, LICENSES[license](pkg, string(years), authors))
    end
    !isempty(file) || info("License file exists, leaving unmodified; use `force=true` to overwrite")
    file
end

function readme(pkg::AbstractString, user::AbstractString=""; force::Bool=false)
    genfile(pkg,"README.md",force) do io
        println(io, "# $pkg")
        isempty(user) && return
        url = "https://travis-ci.org/$user/$pkg.jl"
        println(io, "\n[![Build Status]($url.svg?branch=master)]($url)")
    end
end

function tests(pkg::AbstractString; force::Bool=false)
    genfile(pkg,"test/runtests.jl",force) do io
        print(io, """
        using $pkg
        using Base.Test

        # write your own tests here
        @test 1 == 1
        """)
    end
end

function versionfloor(ver::VersionNumber)
    # return "major.minor" for the most recent release version relative to ver
    # for prereleases with ver.minor == ver.patch == 0, return "major-" since we
    # don't know what the most recent minor version is for the previous major
    if isempty(ver.prerelease) || ver.patch > 0
        return string(ver.major, '.', ver.minor)
    elseif ver.minor > 0
        return string(ver.major, '.', ver.minor - 1)
    else
        return string(ver.major, '-')
    end
end

function require(pkg::AbstractString; force::Bool=false)
    genfile(pkg,"REQUIRE",force) do io
        print(io, """
        julia $(versionfloor(VERSION))
        """)
    end
end

function travis(pkg::AbstractString; force::Bool=false)
    genfile(pkg,".travis.yml",force) do io
        print(io, """
        # Documentation: http://docs.travis-ci.com/user/languages/julia/
        language: julia
        os:
          - linux
          - osx
        julia:
          - release
          - nightly
        notifications:
          email: false
        # uncomment the following lines to override the default test script
        #script:
        #  - if [[ -a .git/shallow ]]; then git fetch --unshallow; fi
        #  - julia -e 'Pkg.clone(pwd()); Pkg.build("$pkg"); Pkg.test("$pkg"; coverage=true)'
        """)
    end
end

function appveyor(pkg::AbstractString; force::Bool=false)
    vf = versionfloor(VERSION)
    if vf[end] == '-' # don't know what previous release was
        vf = string(VERSION.major, '.', VERSION.minor)
        rel32 = "#  - JULIAVERSION: \"julialang/bin/winnt/x86/$vf/julia-$vf-latest-win32.exe\""
        rel64 = "#  - JULIAVERSION: \"julialang/bin/winnt/x64/$vf/julia-$vf-latest-win64.exe\""
    else
        rel32 = "  - JULIAVERSION: \"julialang/bin/winnt/x86/$vf/julia-$vf-latest-win32.exe\""
        rel64 = "  - JULIAVERSION: \"julialang/bin/winnt/x64/$vf/julia-$vf-latest-win64.exe\""
    end
    genfile(pkg,"appveyor.yml",force) do io
        print(io, """
        environment:
          matrix:
        $rel32
        $rel64
          - JULIAVERSION: "julianightlies/bin/winnt/x86/julia-latest-win32.exe"
          - JULIAVERSION: "julianightlies/bin/winnt/x64/julia-latest-win64.exe"

        branches:
          only:
            - master
            - /release-.*/

        notifications:
          - provider: Email
            on_build_success: false
            on_build_failure: false
            on_build_status_changed: false

        install:
        # Download most recent Julia Windows binary
          - ps: (new-object net.webclient).DownloadFile(
                \$("http://s3.amazonaws.com/"+\$env:JULIAVERSION),
                "C:\\projects\\julia-binary.exe")
        # Run installer silently, output to C:\\projects\\julia
          - C:\\projects\\julia-binary.exe /S /D=C:\\projects\\julia

        build_script:
        # Need to convert from shallow to complete for Pkg.clone to work
          - IF EXIST .git\\shallow (git fetch --unshallow)
          - C:\\projects\\julia\\bin\\julia -e "versioninfo();
              Pkg.clone(pwd(), \\"$pkg\\"); Pkg.build(\\"$pkg\\")"

        test_script:
          - C:\\projects\\julia\\bin\\julia --check-bounds=yes -e "Pkg.test(\\"$pkg\\")"
        """)
    end
end

function gitignore(pkg::AbstractString; force::Bool=false)
    genfile(pkg,".gitignore",force) do io
        print(io, """
        *.jl.cov
        *.jl.*.cov
        *.jl.mem
        """)
    end
end

function entrypoint(pkg::AbstractString; force::Bool=false)
    genfile(pkg,"src/$pkg.jl",force) do io
        print(io, """
        module $pkg

        # package code goes here

        end # module
        """)
    end
end

copyright(years::AbstractString, authors::AbstractString) = "> Copyright (c) $years: $authors."

function copyright(years::AbstractString, authors::Array)
    text = "> Copyright (c) $years:"
    for author in authors
        text *= "\n>  * $author"
    end
    return text
end

mit(pkg::AbstractString, years::AbstractString, authors::Union{AbstractString,Array}) =
"""
The $pkg.jl package is licensed under the MIT "Expat" License:

$(copyright(years,authors))
>
> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
>
> The above copyright notice and this permission notice shall be
> included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
> IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
> CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
> TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
> SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
"""

bsd(pkg::AbstractString, years::AbstractString, authors::Union{AbstractString,Array}) =
"""
The $pkg.jl package is licensed under the Simplified "2-clause" BSD License:

$(copyright(years,authors))
>
> Redistribution and use in source and binary forms, with or without
> modification, are permitted provided that the following conditions are
> met:
>
> 1. Redistributions of source code must retain the above copyright
>    notice, this list of conditions and the following disclaimer.
> 2. Redistributions in binary form must reproduce the above copyright
>    notice, this list of conditions and the following disclaimer in the
>    documentation and/or other materials provided with the distribution.
>
> THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
> "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
> LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
> A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
> OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
> SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
> LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
> DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
> THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
> (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
> OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""

asl(pkg::AbstractString, years::AbstractString, authors::Union{AbstractString,Array}) =
"""
The $pkg.jl package is licensed under version 2.0 of the Apache License:

$(copyright(years,authors))
>
>                                 Apache License
>                           Version 2.0, January 2004
>                        http://www.apache.org/licenses/
>
>   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
>
>   1. Definitions.
>
>      "License" shall mean the terms and conditions for use, reproduction,
>      and distribution as defined by Sections 1 through 9 of this document.
>
>      "Licensor" shall mean the copyright owner or entity authorized by
>      the copyright owner that is granting the License.
>
>      "Legal Entity" shall mean the union of the acting entity and all
>      other entities that control, are controlled by, or are under common
>      control with that entity. For the purposes of this definition,
>      "control" means (i) the power, direct or indirect, to cause the
>      direction or management of such entity, whether by contract or
>      otherwise, or (ii) ownership of fifty percent (50%) or more of the
>      outstanding shares, or (iii) beneficial ownership of such entity.
>
>      "You" (or "Your") shall mean an individual or Legal Entity
>      exercising permissions granted by this License.
>
>      "Source" form shall mean the preferred form for making modifications,
>      including but not limited to software source code, documentation
>      source, and configuration files.
>
>      "Object" form shall mean any form resulting from mechanical
>      transformation or translation of a Source form, including but
>      not limited to compiled object code, generated documentation,
>      and conversions to other media types.
>
>      "Work" shall mean the work of authorship, whether in Source or
>      Object form, made available under the License, as indicated by a
>      copyright notice that is included in or attached to the work
>      (an example is provided in the Appendix below).
>
>      "Derivative Works" shall mean any work, whether in Source or Object
>      form, that is based on (or derived from) the Work and for which the
>      editorial revisions, annotations, elaborations, or other modifications
>      represent, as a whole, an original work of authorship. For the purposes
>      of this License, Derivative Works shall not include works that remain
>      separable from, or merely link (or bind by name) to the interfaces of,
>      the Work and Derivative Works thereof.
>
>      "Contribution" shall mean any work of authorship, including
>      the original version of the Work and any modifications or additions
>      to that Work or Derivative Works thereof, that is intentionally
>      submitted to Licensor for inclusion in the Work by the copyright owner
>      or by an individual or Legal Entity authorized to submit on behalf of
>      the copyright owner. For the purposes of this definition, "submitted"
>      means any form of electronic, verbal, or written communication sent
>      to the Licensor or its representatives, including but not limited to
>      communication on electronic mailing lists, source code control systems,
>      and issue tracking systems that are managed by, or on behalf of, the
>      Licensor for the purpose of discussing and improving the Work, but
>      excluding communication that is conspicuously marked or otherwise
>      designated in writing by the copyright owner as "Not a Contribution."
>
>      "Contributor" shall mean Licensor and any individual or Legal Entity
>      on behalf of whom a Contribution has been received by Licensor and
>      subsequently incorporated within the Work.
>
>   2. Grant of Copyright License. Subject to the terms and conditions of
>      this License, each Contributor hereby grants to You a perpetual,
>      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
>      copyright license to reproduce, prepare Derivative Works of,
>      publicly display, publicly perform, sublicense, and distribute the
>      Work and such Derivative Works in Source or Object form.
>
>   3. Grant of Patent License. Subject to the terms and conditions of
>      this License, each Contributor hereby grants to You a perpetual,
>      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
>      (except as stated in this section) patent license to make, have made,
>      use, offer to sell, sell, import, and otherwise transfer the Work,
>      where such license applies only to those patent claims licensable
>      by such Contributor that are necessarily infringed by their
>      Contribution(s) alone or by combination of their Contribution(s)
>      with the Work to which such Contribution(s) was submitted. If You
>      institute patent litigation against any entity (including a
>      cross-claim or counterclaim in a lawsuit) alleging that the Work
>      or a Contribution incorporated within the Work constitutes direct
>      or contributory patent infringement, then any patent licenses
>      granted to You under this License for that Work shall terminate
>      as of the date such litigation is filed.
>
>   4. Redistribution. You may reproduce and distribute copies of the
>      Work or Derivative Works thereof in any medium, with or without
>      modifications, and in Source or Object form, provided that You
>      meet the following conditions:
>
>      (a) You must give any other recipients of the Work or
>          Derivative Works a copy of this License; and
>
>      (b) You must cause any modified files to carry prominent notices
>          stating that You changed the files; and
>
>      (c) You must retain, in the Source form of any Derivative Works
>          that You distribute, all copyright, patent, trademark, and
>          attribution notices from the Source form of the Work,
>          excluding those notices that do not pertain to any part of
>          the Derivative Works; and
>
>      (d) If the Work includes a "NOTICE" text file as part of its
>          distribution, then any Derivative Works that You distribute must
>          include a readable copy of the attribution notices contained
>          within such NOTICE file, excluding those notices that do not
>          pertain to any part of the Derivative Works, in at least one
>          of the following places: within a NOTICE text file distributed
>          as part of the Derivative Works; within the Source form or
>          documentation, if provided along with the Derivative Works; or,
>          within a display generated by the Derivative Works, if and
>          wherever such third-party notices normally appear. The contents
>          of the NOTICE file are for informational purposes only and
>          do not modify the License. You may add Your own attribution
>          notices within Derivative Works that You distribute, alongside
>          or as an addendum to the NOTICE text from the Work, provided
>          that such additional attribution notices cannot be construed
>          as modifying the License.
>
>      You may add Your own copyright statement to Your modifications and
>      may provide additional or different license terms and conditions
>      for use, reproduction, or distribution of Your modifications, or
>      for any such Derivative Works as a whole, provided Your use,
>      reproduction, and distribution of the Work otherwise complies with
>      the conditions stated in this License.
>
>   5. Submission of Contributions. Unless You explicitly state otherwise,
>      any Contribution intentionally submitted for inclusion in the Work
>      by You to the Licensor shall be under the terms and conditions of
>      this License, without any additional terms or conditions.
>      Notwithstanding the above, nothing herein shall supersede or modify
>      the terms of any separate license agreement you may have executed
>      with Licensor regarding such Contributions.
>
>   6. Trademarks. This License does not grant permission to use the trade
>      names, trademarks, service marks, or product names of the Licensor,
>      except as required for reasonable and customary use in describing the
>      origin of the Work and reproducing the content of the NOTICE file.
>
>   7. Disclaimer of Warranty. Unless required by applicable law or
>      agreed to in writing, Licensor provides the Work (and each
>      Contributor provides its Contributions) on an "AS IS" BASIS,
>      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
>      implied, including, without limitation, any warranties or conditions
>      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
>      PARTICULAR PURPOSE. You are solely responsible for determining the
>      appropriateness of using or redistributing the Work and assume any
>      risks associated with Your exercise of permissions under this License.
>
>   8. Limitation of Liability. In no event and under no legal theory,
>      whether in tort (including negligence), contract, or otherwise,
>      unless required by applicable law (such as deliberate and grossly
>      negligent acts) or agreed to in writing, shall any Contributor be
>      liable to You for damages, including any direct, indirect, special,
>      incidental, or consequential damages of any character arising as a
>      result of this License or out of the use or inability to use the
>      Work (including but not limited to damages for loss of goodwill,
>      work stoppage, computer failure or malfunction, or any and all
>      other commercial damages or losses), even if such Contributor
>      has been advised of the possibility of such damages.
>
>   9. Accepting Warranty or Additional Liability. While redistributing
>      the Work or Derivative Works thereof, You may choose to offer,
>      and charge a fee for, acceptance of support, warranty, indemnity,
>      or other liability obligations and/or rights consistent with this
>      License. However, in accepting such obligations, You may act only
>      on Your own behalf and on Your sole responsibility, not on behalf
>      of any other Contributor, and only if You agree to indemnify,
>      defend, and hold each Contributor harmless for any liability
>      incurred by, or claims asserted against, such Contributor by reason
>      of your accepting any such warranty or additional liability.
"""

mpl(pkg::AbstractString, years::AbstractString, authors::Union{AbstractString,Array}) =
"""
The $pkg.jl package is licensed under the Mozilla Public License, Version 2.0:

$(copyright(years,authors))
>
>   Mozilla Public License Version 2.0
>   ==================================
>
>   1. Definitions
>   --------------
>
>   1.1. "Contributor"
>       means each individual or legal entity that creates, contributes to
>       the creation of, or owns Covered Software.
>
>   1.2. "Contributor Version"
>       means the combination of the Contributions of others (if any) used
>       by a Contributor and that particular Contributor's Contribution.
>
>   1.3. "Contribution"
>       means Covered Software of a particular Contributor.
>
>   1.4. "Covered Software"
>       means Source Code Form to which the initial Contributor has attached
>       the notice in Exhibit A, the Executable Form of such Source Code
>       Form, and Modifications of such Source Code Form, in each case
>       including portions thereof.
>
>   1.5. "Incompatible With Secondary Licenses"
>       means
>
>       (a) that the initial Contributor has attached the notice described
>           in Exhibit B to the Covered Software; or
>
>       (b) that the Covered Software was made available under the terms of
>           version 1.1 or earlier of the License, but not also under the
>           terms of a Secondary License.
>
>   1.6. "Executable Form"
>       means any form of the work other than Source Code Form.
>
>   1.7. "Larger Work"
>       means a work that combines Covered Software with other material, in
>       a separate file or files, that is not Covered Software.
>
>   1.8. "License"
>       means this document.
>
>   1.9. "Licensable"
>       means having the right to grant, to the maximum extent possible,
>       whether at the time of the initial grant or subsequently, any and
>       all of the rights conveyed by this License.
>
>   1.10. "Modifications"
>       means any of the following:
>
>       (a) any file in Source Code Form that results from an addition to,
>           deletion from, or modification of the contents of Covered
>           Software; or
>
>       (b) any new file in Source Code Form that contains any Covered
>           Software.
>
>   1.11. "Patent Claims" of a Contributor
>       means any patent claim(s), including without limitation, method,
>       process, and apparatus claims, in any patent Licensable by such
>       Contributor that would be infringed, but for the grant of the
>       License, by the making, using, selling, offering for sale, having
>       made, import, or transfer of either its Contributions or its
>       Contributor Version.
>
>   1.12. "Secondary License"
>       means either the GNU General Public License, Version 2.0, the GNU
>       Lesser General Public License, Version 2.1, the GNU Affero General
>       Public License, Version 3.0, or any later versions of those
>       licenses.
>
>   1.13. "Source Code Form"
>       means the form of the work preferred for making modifications.
>
>   1.14. "You" (or "Your")
>       means an individual or a legal entity exercising rights under this
>       License. For legal entities, "You" includes any entity that
>       controls, is controlled by, or is under common control with You. For
>       purposes of this definition, "control" means (a) the power, direct
>       or indirect, to cause the direction or management of such entity,
>       whether by contract or otherwise, or (b) ownership of more than
>       fifty percent (50%) of the outstanding shares or beneficial
>       ownership of such entity.
>
>   2. License Grants and Conditions
>   --------------------------------
>
>   2.1. Grants
>
>   Each Contributor hereby grants You a world-wide, royalty-free,
>   non-exclusive license:
>
>   (a) under intellectual property rights (other than patent or trademark)
>       Licensable by such Contributor to use, reproduce, make available,
>       modify, display, perform, distribute, and otherwise exploit its
>       Contributions, either on an unmodified basis, with Modifications, or
>       as part of a Larger Work; and
>
>   (b) under Patent Claims of such Contributor to make, use, sell, offer
>       for sale, have made, import, and otherwise transfer either its
>       Contributions or its Contributor Version.
>
>   2.2. Effective Date
>
>   The licenses granted in Section 2.1 with respect to any Contribution
>   become effective for each Contribution on the date the Contributor first
>   distributes such Contribution.
>
>   2.3. Limitations on Grant Scope
>
>   The licenses granted in this Section 2 are the only rights granted under
>   this License. No additional rights or licenses will be implied from the
>   distribution or licensing of Covered Software under this License.
>   Notwithstanding Section 2.1(b) above, no patent license is granted by a
>   Contributor:
>
>   (a) for any code that a Contributor has removed from Covered Software;
>       or
>
>   (b) for infringements caused by: (i) Your and any other third party's
>       modifications of Covered Software, or (ii) the combination of its
>       Contributions with other software (except as part of its Contributor
>       Version); or
>
>   (c) under Patent Claims infringed by Covered Software in the absence of
>       its Contributions.
>
>   This License does not grant any rights in the trademarks, service marks,
>   or logos of any Contributor (except as may be necessary to comply with
>   the notice requirements in Section 3.4).
>
>   2.4. Subsequent Licenses
>
>   No Contributor makes additional grants as a result of Your choice to
>   distribute the Covered Software under a subsequent version of this
>   License (see Section 10.2) or under the terms of a Secondary License (if
>   permitted under the terms of Section 3.3).
>
>   2.5. Representation
>
>   Each Contributor represents that the Contributor believes its
>   Contributions are its original creation(s) or it has sufficient rights
>   to grant the rights to its Contributions conveyed by this License.
>
>   2.6. Fair Use
>
>   This License is not intended to limit any rights You have under
>   applicable copyright doctrines of fair use, fair dealing, or other
>   equivalents.
>
>   2.7. Conditions
>
>   Sections 3.1, 3.2, 3.3, and 3.4 are conditions of the licenses granted
>   in Section 2.1.
>
>   3. Responsibilities
>   -------------------
>
>   3.1. Distribution of Source Form
>
>   All distribution of Covered Software in Source Code Form, including any
>   Modifications that You create or to which You contribute, must be under
>   the terms of this License. You must inform recipients that the Source
>   Code Form of the Covered Software is governed by the terms of this
>   License, and how they can obtain a copy of this License. You may not
>   attempt to alter or restrict the recipients' rights in the Source Code
>   Form.
>
>   3.2. Distribution of Executable Form
>
>   If You distribute Covered Software in Executable Form then:
>
>   (a) such Covered Software must also be made available in Source Code
>       Form, as described in Section 3.1, and You must inform recipients of
>       the Executable Form how they can obtain a copy of such Source Code
>       Form by reasonable means in a timely manner, at a charge no more
>       than the cost of distribution to the recipient; and
>
>   (b) You may distribute such Executable Form under the terms of this
>       License, or sublicense it under different terms, provided that the
>       license for the Executable Form does not attempt to limit or alter
>       the recipients' rights in the Source Code Form under this License.
>
>   3.3. Distribution of a Larger Work
>
>   You may create and distribute a Larger Work under terms of Your choice,
>   provided that You also comply with the requirements of this License for
>   the Covered Software. If the Larger Work is a combination of Covered
>   Software with a work governed by one or more Secondary Licenses, and the
>   Covered Software is not Incompatible With Secondary Licenses, this
>   License permits You to additionally distribute such Covered Software
>   under the terms of such Secondary License(s), so that the recipient of
>   the Larger Work may, at their option, further distribute the Covered
>   Software under the terms of either this License or such Secondary
>   License(s).
>
>   3.4. Notices
>
>   You may not remove or alter the substance of any license notices
>   (including copyright notices, patent notices, disclaimers of warranty,
>   or limitations of liability) contained within the Source Code Form of
>   the Covered Software, except that You may alter any license notices to
>   the extent required to remedy known factual inaccuracies.
>
>   3.5. Application of Additional Terms
>
>   You may choose to offer, and to charge a fee for, warranty, support,
>   indemnity or liability obligations to one or more recipients of Covered
>   Software. However, You may do so only on Your own behalf, and not on
>   behalf of any Contributor. You must make it absolutely clear that any
>   such warranty, support, indemnity, or liability obligation is offered by
>   You alone, and You hereby agree to indemnify every Contributor for any
>   liability incurred by such Contributor as a result of warranty, support,
>   indemnity or liability terms You offer. You may include additional
>   disclaimers of warranty and limitations of liability specific to any
>   jurisdiction.
>
>   4. Inability to Comply Due to Statute or Regulation
>   ---------------------------------------------------
>
>   If it is impossible for You to comply with any of the terms of this
>   License with respect to some or all of the Covered Software due to
>   statute, judicial order, or regulation then You must: (a) comply with
>   the terms of this License to the maximum extent possible; and (b)
>   describe the limitations and the code they affect. Such description must
>   be placed in a text file included with all distributions of the Covered
>   Software under this License. Except to the extent prohibited by statute
>   or regulation, such description must be sufficiently detailed for a
>   recipient of ordinary skill to be able to understand it.
>
>   5. Termination
>   --------------
>
>   5.1. The rights granted under this License will terminate automatically
>   if You fail to comply with any of its terms. However, if You become
>   compliant, then the rights granted under this License from a particular
>   Contributor are reinstated (a) provisionally, unless and until such
>   Contributor explicitly and finally terminates Your grants, and (b) on an
>   ongoing basis, if such Contributor fails to notify You of the
>   non-compliance by some reasonable means prior to 60 days after You have
>   come back into compliance. Moreover, Your grants from a particular
>   Contributor are reinstated on an ongoing basis if such Contributor
>   notifies You of the non-compliance by some reasonable means, this is the
>   first time You have received notice of non-compliance with this License
>   from such Contributor, and You become compliant prior to 30 days after
>   Your receipt of the notice.
>
>   5.2. If You initiate litigation against any entity by asserting a patent
>   infringement claim (excluding declaratory judgment actions,
>   counter-claims, and cross-claims) alleging that a Contributor Version
>   directly or indirectly infringes any patent, then the rights granted to
>   You by any and all Contributors for the Covered Software under Section
>   2.1 of this License shall terminate.
>
>   5.3. In the event of termination under Sections 5.1 or 5.2 above, all
>   end user license agreements (excluding distributors and resellers) which
>   have been validly granted by You or Your distributors under this License
>   prior to termination shall survive termination.
>
>   ************************************************************************
>   *                                                                      *
>   *  6. Disclaimer of Warranty                                           *
>   *  -------------------------                                           *
>   *                                                                      *
>   *  Covered Software is provided under this License on an "as is"       *
>   *  basis, without warranty of any kind, either expressed, implied, or  *
>   *  statutory, including, without limitation, warranties that the       *
>   *  Covered Software is free of defects, merchantable, fit for a        *
>   *  particular purpose or non-infringing. The entire risk as to the     *
>   *  quality and performance of the Covered Software is with You.        *
>   *  Should any Covered Software prove defective in any respect, You     *
>   *  (not any Contributor) assume the cost of any necessary servicing,   *
>   *  repair, or correction. This disclaimer of warranty constitutes an   *
>   *  essential part of this License. No use of any Covered Software is   *
>   *  authorized under this License except under this disclaimer.         *
>   *                                                                      *
>   ************************************************************************
>
>   ************************************************************************
>   *                                                                      *
>   *  7. Limitation of Liability                                          *
>   *  --------------------------                                          *
>   *                                                                      *
>   *  Under no circumstances and under no legal theory, whether tort      *
>   *  (including negligence), contract, or otherwise, shall any           *
>   *  Contributor, or anyone who distributes Covered Software as          *
>   *  permitted above, be liable to You for any direct, indirect,         *
>   *  special, incidental, or consequential damages of any character      *
>   *  including, without limitation, damages for lost profits, loss of    *
>   *  goodwill, work stoppage, computer failure or malfunction, or any    *
>   *  and all other commercial damages or losses, even if such party      *
>   *  shall have been informed of the possibility of such damages. This   *
>   *  limitation of liability shall not apply to liability for death or   *
>   *  personal injury resulting from such party's negligence to the       *
>   *  extent applicable law prohibits such limitation. Some               *
>   *  jurisdictions do not allow the exclusion or limitation of           *
>   *  incidental or consequential damages, so this exclusion and          *
>   *  limitation may not apply to You.                                    *
>   *                                                                      *
>   ************************************************************************
>
>   8. Litigation
>   -------------
>
>   Any litigation relating to this License may be brought only in the
>   courts of a jurisdiction where the defendant maintains its principal
>   place of business and such litigation shall be governed by laws of that
>   jurisdiction, without reference to its conflict-of-law provisions.
>   Nothing in this Section shall prevent a party's ability to bring
>   cross-claims or counter-claims.
>
>   9. Miscellaneous
>   ----------------
>
>   This License represents the complete agreement concerning the subject
>   matter hereof. If any provision of this License is held to be
>   unenforceable, such provision shall be reformed only to the extent
>   necessary to make it enforceable. Any law or regulation which provides
>   that the language of a contract shall be construed against the drafter
>   shall not be used to construe this License against a Contributor.
>
>   10. Versions of the License
>   ---------------------------
>
>   10.1. New Versions
>
>   Mozilla Foundation is the license steward. Except as provided in Section
>   10.3, no one other than the license steward has the right to modify or
>   publish new versions of this License. Each version will be given a
>   distinguishing version number.
>
>   10.2. Effect of New Versions
>
>   You may distribute the Covered Software under the terms of the version
>   of the License under which You originally received the Covered Software,
>   or under the terms of any subsequent version published by the license
>   steward.
>
>   10.3. Modified Versions
>
>   If you create software not governed by this License, and you want to
>   create a new license for such software, you may create and use a
>   modified version of this License if you rename the license and remove
>   any references to the name of the license steward (except to note that
>   such modified license differs from this License).
>
>   10.4. Distributing Source Code Form that is Incompatible With Secondary
>   Licenses
>
>   If You choose to distribute Source Code Form that is Incompatible With
>   Secondary Licenses under the terms of this version of the License, the
>   notice described in Exhibit B of this License must be attached.
>
>   Exhibit A - Source Code Form License Notice
>   -------------------------------------------
>
>     This Source Code Form is subject to the terms of the Mozilla Public
>     License, v. 2.0. If a copy of the MPL was not distributed with this
>     file, You can obtain one at http://mozilla.org/MPL/2.0/.
>
>   If it is not possible or desirable to put the notice in a particular
>   file, then You may include the notice in a location (such as a LICENSE
>   file in a relevant directory) where a recipient would be likely to look
>   for such a notice.
>
>   You may add additional accurate notices of copyright ownership.
>
>   Exhibit B - "Incompatible With Secondary Licenses" Notice
>   ---------------------------------------------------------
>
>     This Source Code Form is "Incompatible With Secondary Licenses", as
>     defined by the Mozilla Public License, v. 2.0.
"""

const LICENSES = Dict("MIT" => mit, "BSD" => bsd, "ASL" => asl, "MPL" => mpl)

end # module
