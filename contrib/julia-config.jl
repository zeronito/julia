const options =
[
   "--cflags",
   "--ldflags"
];

function libdir()
   opts = Base.compileropts();
   image_path = bytestring(opts.image_file);
   @windows_only dir = joinpath(Base.JULIA_HOME, "julia");
   @unix_only dir = match(r"(.*)(sys.ji)",image_path).captures[1];
end

function includedir()
   dir = match(r"(.*)(bin)",JULIA_HOME).captures[1]*"include/julia";
end

function ldflags()
   @unix_only "-L$(libdir()) -Wl,-rpath $(libdir()) -ljulia";
end

function cflags()
   @unix_only "-I$(includedir())";
end

function check_args(args)
   checked = intersect(args,options);
   if length(checked) == 0 || length(checked) != length(args)
      println(STDERR,"Usage: julia-config [",reduce((x,y)->"$x|$y",options),"]");
      exit(1);
   end
end

function main()
   check_args(ARGS);
   for args in ARGS
      if args == "--ldflags"
         println(ldflags());
      elseif args == "--cflags"
         println(cflags());
      end
   end
end

main();
