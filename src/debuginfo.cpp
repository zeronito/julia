#if USE_MCJIT
typedef object::SymbolRef SymRef;
#endif

// --- storing and accessing source location metadata ---

#ifndef USE_MCJIT
struct FuncInfo{
    const Function* func;
    size_t lengthAdr;
    std::string name;
    std::string filename;
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    PRUNTIME_FUNCTION fnentry;
#endif
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> lines;
};
#else
struct ObjectInfo {
    object::ObjectFile* object;
    object::SymbolRef symref;
    size_t size;
};
#endif

#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
#include <dbghelp.h>
extern "C" EXCEPTION_DISPOSITION _seh_exception_handler(PEXCEPTION_RECORD ExceptionRecord,void *EstablisherFrame, PCONTEXT ContextRecord, void *DispatcherContext);
extern "C" volatile int jl_in_stackwalk;
#endif

struct revcomp {
    bool operator() (const size_t& lhs, const size_t& rhs) const
    { return lhs>rhs; }
};

class JuliaJITEventListener: public JITEventListener
{
#ifndef USE_MCJIT
    std::map<size_t, FuncInfo, revcomp> info;
#else
    std::map<size_t, ObjectInfo, revcomp> objectmap;
#endif

public:
    JuliaJITEventListener(){}
    virtual ~JuliaJITEventListener() {}

#ifndef USE_MCJIT
    virtual void NotifyFunctionEmitted(const Function &F, void *Code,
                                       size_t Size, const EmittedFunctionDetails &Details)
    {
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
        assert(!jl_in_stackwalk);
        jl_in_stackwalk = 1;
        uintptr_t catchjmp = (uintptr_t)Code+Size;
        *(uint8_t*)(catchjmp+0) = 0x48;
        *(uint8_t*)(catchjmp+1) = 0xb8; // mov RAX, QWORD PTR [...]
        *(uint64_t*)(catchjmp+2) = (uint64_t)&_seh_exception_handler;
        *(uint8_t*)(catchjmp+10) = 0xff;
        *(uint8_t*)(catchjmp+11) = 0xe0; // jmp RAX
        PRUNTIME_FUNCTION tbl = (PRUNTIME_FUNCTION)((catchjmp+12+3)&~(uintptr_t)3);
        uint8_t *UnwindData = (uint8_t*)((((uintptr_t)&tbl[1])+3)&~(uintptr_t)3);
        RUNTIME_FUNCTION fn = {0,(DWORD)Size+13,(DWORD)(intptr_t)(UnwindData-(uint8_t*)Code)};
        tbl[0] = fn;
        UnwindData[0] = 0x09; // version info, UNW_FLAG_EHANDLER
        UnwindData[1] = 4;    // size of prolog (bytes)
        UnwindData[2] = 2;    // count of unwind codes (slots)
        UnwindData[3] = 0x05; // frame register (rbp) = rsp
        UnwindData[4] = 4;    // second instruction
        UnwindData[5] = 0x03; // mov RBP, RSP
        UnwindData[6] = 1;    // first instruction
        UnwindData[7] = 0x50; // push RBP
        *(DWORD*)&UnwindData[8] = (DWORD)(catchjmp-(intptr_t)Code);
        DWORD mod_size = (DWORD)(size_t)(&UnwindData[8]-(uint8_t*)Code);
        if (!SymLoadModuleEx(GetCurrentProcess(), NULL, NULL, NULL, (DWORD64)Code, mod_size, NULL, SLMFLAG_VIRTUAL)) {
            static int warned = 0;
            if (!warned) {
                JL_PRINTF(JL_STDERR, "WARNING: failed to insert function info for backtrace\n");
                warned = 1;
            }
        }
        else {
            if (!SymAddSymbol(GetCurrentProcess(), (ULONG64)Code, F.getName().data(), (DWORD64)Code, mod_size, 0)) {
                JL_PRINTF(JL_STDERR, "WARNING: failed to insert function name into debug info\n");
            }
            if (!RtlAddFunctionTable(tbl,1,(DWORD64)Code)) {
                JL_PRINTF(JL_STDERR, "WARNING: failed to insert function stack unwind info\n");
            }
        }
        jl_in_stackwalk = 0;

        FuncInfo tmp = {&F, Size, std::string(), std::string(), tbl, Details.LineStarts};
#else
        FuncInfo tmp = {&F, Size, std::string(), std::string(), Details.LineStarts};
#endif
        if (tmp.lines.size() != 0) info[(size_t)(Code)] = tmp;
    }

    std::map<size_t, FuncInfo, revcomp>& getMap()
    {
        return info;
    }
#endif // ndef USE_MCJIT

#if USE_MCJIT
    virtual void NotifyObjectEmitted(const ObjectImage &obj)
    {
        uint64_t Addr;
        object::SymbolRef::Type SymbolType;

        #ifdef LLVM35
        for (const object::SymbolRef &sym_iter : obj.symbols()) {
            sym_iter.getType(SymbolType);
            if (SymbolType != object::SymbolRef::ST_Function) continue;
            sym_iter.getAddress(Addr);
            ObjectInfo tmp = {obj.getObjectFile(), sym_iter, obj.getData().size()};
            objectmap[Addr] = tmp;
        }
        #else
        error_code itererr; 
        object::symbol_iterator sym_iter = obj.begin_symbols();
        object::symbol_iterator sym_end = obj.end_symbols();
        for (; sym_iter != sym_end; sym_iter.increment(itererr)) {
            sym_iter->getType(SymbolType);
            if (SymbolType != object::SymbolRef::ST_Function) continue;
            sym_iter->getAddress(Addr);

            ObjectInfo tmp = {obj.getObjectFile(), *sym_iter};
            objectmap[Addr] = tmp;
        }
        #endif
    }

    // must implement if we ever start freeing code
    // virtual void NotifyFreeingObject(const ObjectImage &obj) {}

    std::map<size_t, ObjectInfo, revcomp>& getObjectMap()
    {
        return objectmap;
    }
#endif // USE_MCJIT
};

extern "C"
const char *jl_demangle(const char *name)
{
    const char *start = name;
    const char *end = start;
    char *ret;
    while ((*start++ != '_') && (*start != '\0'));
    if (*name == '\0') goto done;
    while ((*end++ != ';') && (*end != '\0'));
    if (*name == '\0') goto done;
    ret = (char*)malloc(end-start);
    memcpy(ret,start,end-start-1);
    ret[end-start-1] = '\0';
    return ret;
 done:
    return strdup(name);
}

JuliaJITEventListener *jl_jit_events;

extern "C" void jl_getFunctionInfo(const char **name, int *line, const char **filename, uintptr_t pointer, int skipC);

void lookup_pointer(DIContext *context, const char **name, int *line, const char **filename, size_t pointer, int demangle)
{
    if (context == NULL) return;
    #ifdef LLVM35
    DILineInfoSpecifier infoSpec(DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath,
                                 DILineInfoSpecifier::FunctionNameKind::ShortName);
    #else
    int infoSpec = DILineInfoSpecifier::FileLineInfo |
                   DILineInfoSpecifier::AbsoluteFilePath |
                   DILineInfoSpecifier::FunctionName;
    #endif
    DILineInfo info = context->getLineInfoForAddress(pointer, infoSpec);

    #ifndef LLVM35 // LLVM <= 3.4
    if (strcmp(info.getFunctionName(), "<invalid>") == 0) return;
    if (demangle)
	*name = jl_demangle(info.getFunctionName());
    else
	*name = strdup(info.getFunctionName());
    *line = info.getLine();
    *filename = strdup(info.getFileName());
    #else
    if (strcmp(info.FunctionName.c_str(), "<invalid>") == 0) return;
    *name = strdup(info.FunctionName.c_str());
    *line = info.Line;
    *filename = strdup(info.FileName.c_str());
    #endif
}

#ifdef _OS_DARWIN_
#include <mach-o/dyld.h>
#endif
#ifndef _OS_WINDOWS_
#include <dlfcn.h>
#endif
typedef struct {
    llvm::object::ObjectFile *obj;
    DIContext *ctx;
    int64_t slide;
} objfileentry_t;
typedef std::map<uint64_t, objfileentry_t> obfiletype;
static obfiletype objfilemap;

#ifdef _OS_DARWIN_
bool getObjUUID(llvm::object::MachOObjectFile *obj, uint8_t uuid[16])
{
#ifdef LLVM35
    uint32_t LoadCommandCount = obj->getHeader().ncmds;
#else
    uint32_t LoadCommandCount = obj->getHeader().NumLoadCommands;
#endif
    llvm::object::MachOObjectFile::LoadCommandInfo Load = obj->getFirstLoadCommandInfo();
    for (unsigned I = 0; ; ++I) {
        if (
#ifdef LLVM35
            Load.C.cmd == LC_UUID
#else
            Load.C.Type == LC_UUID
#endif
            ) {
            memcpy(uuid,((MachO::uuid_command*)Load.Ptr)->uuid,16);
            return true;
        }
        else if (I == LoadCommandCount - 1) {
            return false;
        }
        else {
            Load = obj->getNextLoadCommandInfo(Load);
        }
    }
}
#endif

extern char *jl_sysimage_name;

bool jl_is_sysimg(const char *path)
{
    if (!jl_sysimage_name)
        return 0;
    const char *filename = strrchr(path,'/');
    if (filename == NULL)
        filename = path;
    const char *sysimgname = strrchr(jl_sysimage_name,'/');
    if (sysimgname == NULL)
        sysimgname = jl_sysimage_name;
    return strncmp(filename,sysimgname,strrchr(path,'.')-filename) == 0;
}

#if defined(_OS_WINDOWS_) && !defined(USE_MCJIT)
void jl_getDylibFunctionInfo(const char **name, int *line, const char **filename, size_t pointer, int skipC)
{
    return;
}
#else
void jl_getDylibFunctionInfo(const char **name, int *line, const char **filename, size_t pointer, int skipC)
{
#ifdef _OS_WINDOWS_
    DWORD fbase = SymGetModuleBase64(GetCurrentProcess(),(DWORD)pointer);
    if (fbase != 0) {
#else
    Dl_info dlinfo;
    if ((dladdr((void*)pointer, &dlinfo) != 0) && dlinfo.dli_fname) {
        if (skipC && !jl_is_sysimg(dlinfo.dli_fname))
            return;
        uint64_t fbase = (uint64_t)dlinfo.dli_fbase;
#endif
        obfiletype::iterator it = objfilemap.find(fbase);
        llvm::object::ObjectFile *obj = NULL;
        DIContext *context = NULL;
        int64_t slide = 0;
        if (it == objfilemap.end()) {
#ifdef _OS_DARWIN_
            // First find the uuid of the object file (we'll use this to make sure we find the
            // correct debug symbol file).
            uint8_t uuid[16], uuid2[16];
#ifdef LLVM35
            ErrorOr<llvm::object::ObjectFile*> origerrorobj = llvm::object::ObjectFile::createObjectFile(
#else
            llvm::object::ObjectFile *origerrorobj = llvm::object::ObjectFile::createObjectFile(
#endif
                    MemoryBuffer::getMemBuffer(
                    StringRef((const char *)fbase, (size_t)(((uint64_t)-1)-fbase)),"",false)
#ifdef LLVM35
                    ,false, sys::fs::file_magic::unknown
#endif
            );
            if (!origerrorobj) {
                objfileentry_t entry = {obj,context,slide};
                objfilemap[fbase] = entry;
                return;
            }
#ifdef LLVM35
            llvm::object::MachOObjectFile *morigobj = (llvm::object::MachOObjectFile *)origerrorobj.get();
#else
            llvm::object::MachOObjectFile *morigobj = (llvm::object::MachOObjectFile *)origerrorobj;
#endif
            if (!getObjUUID(morigobj,uuid)) {
                objfileentry_t entry = {obj,context,slide};
                objfilemap[fbase] = entry;
                return;
            }

            // On OS X debug symbols are not contained in the dynamic library and that's why
            // we can't have nice things (easily). For now we only support .dSYM files in the same directory
            // as the shared library. In the future we may use DBGCopyFullDSYMURLForUUID from CoreFoundation to make
            // use of spotlight to find the .dSYM file.
            char dsympath[PATH_MAX];
            strlcpy(dsympath, dlinfo.dli_fname, sizeof(dsympath));
            strlcat(dsympath, ".dSYM/Contents/Resources/DWARF/", sizeof(dsympath));
            strlcat(dsympath, strrchr(dlinfo.dli_fname,'/')+1, sizeof(dsympath));
#ifdef LLVM35
            ErrorOr<llvm::object::ObjectFile*> errorobj = llvm::object::ObjectFile::createObjectFile(dsympath);
#else
            llvm::object::ObjectFile *errorobj = llvm::object::ObjectFile::createObjectFile(dsympath);
#endif
#else
#ifndef _OS_WINDOWS_
            const char *fname = dlinfo.dli_fname;
#else
            IMAGEHLP_MODULE64 ModuleInfo;
            ModuleInfo.SizeOfStruct = sizeof(IMAGEHLP_MODULE64);
            SymGetModuleInfo64(GetCurrentProcess(), (DWORD64)pointer, &ModuleInfo);
            char *fname = ModuleInfo.LoadedImageName;
            JL_PRINTF(JL_STDOUT,fname);
#endif
            // On non OS X systems we need to mmap another copy because of the permissions on the mmaped
            // shared library.
#ifdef LLVM35
            ErrorOr<llvm::object::ObjectFile*> errorobj = llvm::object::ObjectFile::createObjectFile(fname);
#else
            llvm::object::ObjectFile *errorobj = llvm::object::ObjectFile::createObjectFile(fname);
#endif
#endif
#ifdef LLVM35
            if (errorobj) {
                obj = errorobj.get();
#else
            if (errorobj != NULL) {
                obj = errorobj;
#endif
#ifdef _OS_DARWIN_
                if (getObjUUID(morigobj,uuid2) && memcmp(uuid,uuid2,sizeof(uuid)) == 0) {
#endif
                    context = DIContext::getDWARFContext(obj);
                    slide = -(uint64_t)fbase;
#ifdef _OS_DARWIN_
                }
#endif
#ifdef _OS_WINDOWS_
                assert(obj->isCOFF());
                llvm::object::COFFObjectFile *coffobj = (llvm::object::COFFObjectFile *)obj;
                const llvm::object::pe32plus_header *pe32plus;
                coffobj->getPE32PlusHeader(pe32plus);
                if (pe32plus != NULL) {
                    slide = pe32plus->ImageBase-fbase;
                }
                else {
                    const llvm::object::pe32_header *pe32;
                    coffobj->getPE32Header(pe32); 
                    if (pe32 == NULL) {
                        obj = NULL;
                        context = NULL;
                    }
                    else {
                        slide = pe32->ImageBase-fbase;
                    }
                }
#endif

            }
            objfileentry_t entry = {obj,context,slide};
            objfilemap[fbase] = entry;
        }
        else {
            obj = it->second.obj;
            context = it->second.ctx;
            slide = it->second.slide;
        }

        lookup_pointer(context, name, line, filename, pointer+slide, jl_is_sysimg(dlinfo.dli_fname));
    }
    return;
}
#endif

void jl_getFunctionInfo(const char **name, int *line, const char **filename, size_t pointer, int skipC)
{
    *name = NULL;
    *line = -1;
    *filename = "no file";

#if USE_MCJIT
// With MCJIT we can get function information directly from the ObjectFile
    std::map<size_t, ObjectInfo, revcomp> &objmap = jl_jit_events->getObjectMap();
    std::map<size_t, ObjectInfo, revcomp>::iterator it = objmap.lower_bound(pointer);
    llvm::object::ObjectFile *Obj = it->second.object;

    if (it == objmap.end())
        return jl_getDylibFunctionInfo(name,line,filename,pointer,skipC);
    if ((pointer - it->first) > it->second.size)
        return jl_getDylibFunctionInfo(name,line,filename,pointer,skipC);

    DIContext *context = DIContext::getDWARFContext(it->second.object);
    lookup_pointer(context, name, line, filename, pointer, 1);

#else // !USE_MCJIT

// Without MCJIT we use the FuncInfo structure containing address maps
    std::map<size_t, FuncInfo, revcomp> &info = jl_jit_events->getMap();
    std::map<size_t, FuncInfo, revcomp>::iterator it = info.lower_bound(pointer);
    if (it != info.end() && (size_t)(*it).first + (*it).second.lengthAdr >= pointer) {
        // commenting these lines out skips functions that don't
        // have explicit debug info. this is useful for hiding
        // the jlcall wrapper functions we generate.
#if LLVM_VERSION_MAJOR == 3
#if LLVM_VERSION_MINOR == 0
        //*name = &(*(*it).second.func).getNameStr()[0];
#elif LLVM_VERSION_MINOR >= 1
        //*name = (((*(*it).second.func).getName()).data());
#endif
#endif
        std::vector<JITEvent_EmittedFunctionDetails::LineStart>::iterator vit =
            (*it).second.lines.begin();
        JITEvent_EmittedFunctionDetails::LineStart prev = *vit;

        if ((*it).second.func) {
            DISubprogram debugscope =
                DISubprogram(prev.Loc.getScope((*it).second.func->getContext()));
            *filename = debugscope.getFilename().data();
            // the DISubprogram has the un-mangled name, so use that if
            // available.
            *name = debugscope.getName().data();
        }
        else {
            *name = (*it).second.name.c_str();
            *filename = (*it).second.filename.c_str();
        }

        vit++;

        while (vit != (*it).second.lines.end()) {
            if (pointer <= (*vit).Address) {
                *line = prev.Loc.getLine();
                break;
            }
            prev = *vit;
            vit++;
        }
        if (*line == -1) {
            *line = prev.Loc.getLine();
        }
    }
    else {
        jl_getDylibFunctionInfo(name,line,filename,pointer,skipC);
    }
#endif // USE_MCJIT
}


#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
extern "C" void* CALLBACK jl_getUnwindInfo(HANDLE hProcess, ULONG64 AddrBase, ULONG64 UserContext);
#ifndef USE_MCJIT

void* CALLBACK jl_getUnwindInfo(HANDLE hProcess, ULONG64 AddrBase, ULONG64 UserContext)
{
    std::map<size_t, FuncInfo, revcomp> &info = jl_jit_events->getMap();
    std::map<size_t, FuncInfo, revcomp>::iterator it = info.lower_bound(AddrBase);
    if (it != info.end() && (size_t)(*it).first + (*it).second.lengthAdr >= AddrBase) {
        return (void*)(*it).second.fnentry;
    }
    return NULL;
}

// Custom memory manager for exception handling on Windows
// we overallocate 48 bytes at the end of each function
// for unwind information (see NotifyFunctionEmitted)
class JITMemoryManagerWin : public JITMemoryManager {
private:
  JITMemoryManager *JMM;
public:
  JITMemoryManagerWin() : JITMemoryManager() {
      JMM = JITMemoryManager::CreateDefaultMemManager();
  }
  virtual void setMemoryWritable() { return JMM->setMemoryWritable(); }
  virtual void setMemoryExecutable() { return JMM->setMemoryExecutable(); }
  virtual void setPoisonMemory(bool poison) { return JMM->setPoisonMemory(poison); }
  virtual void AllocateGOT() { JMM->AllocateGOT(); HasGOT = true; }
  virtual uint8_t *getGOTBase() const { return JMM->getGOTBase(); }
  virtual uint8_t *startFunctionBody(const Function *F,
                                     uintptr_t &ActualSize) { ActualSize += 48; uint8_t *ret = JMM->startFunctionBody(F,ActualSize); ActualSize -= 48; return ret; }
  virtual uint8_t *allocateStub(const GlobalValue* F, unsigned StubSize,
                                unsigned Alignment)  { return JMM->allocateStub(F,StubSize,Alignment); }
  virtual void endFunctionBody(const Function *F, uint8_t *FunctionStart,
                               uint8_t *FunctionEnd) { return JMM->endFunctionBody(F,FunctionStart,FunctionEnd+48); }
  virtual uint8_t *allocateSpace(intptr_t Size, unsigned Alignment) { return JMM->allocateSpace(Size,Alignment); }
  virtual uint8_t *allocateGlobal(uintptr_t Size, unsigned Alignment) { return JMM->allocateGlobal(Size,Alignment); }
  virtual void deallocateFunctionBody(void *Body) { return JMM->deallocateFunctionBody(Body); }
  virtual uint8_t* startExceptionTable(const Function* F,
                                       uintptr_t &ActualSize) { return JMM->startExceptionTable(F,ActualSize); }
  virtual void endExceptionTable(const Function *F, uint8_t *TableStart,
                                 uint8_t *TableEnd, uint8_t* FrameRegister) { return JMM->endExceptionTable(F,TableStart,TableEnd,FrameRegister); }
  virtual void deallocateExceptionTable(void *ET) { return JMM->deallocateExceptionTable(ET); }
  virtual bool CheckInvariants(std::string &str) { return JMM->CheckInvariants(str); }
  virtual size_t GetDefaultCodeSlabSize() { return JMM->GetDefaultCodeSlabSize(); }
  virtual size_t GetDefaultDataSlabSize() { return JMM->GetDefaultDataSlabSize(); }
  virtual size_t GetDefaultStubSlabSize() { return JMM->GetDefaultStubSlabSize(); }
  virtual unsigned GetNumCodeSlabs() { return JMM->GetNumCodeSlabs(); }
  virtual unsigned GetNumDataSlabs() { return JMM->GetNumDataSlabs(); }
  virtual unsigned GetNumStubSlabs() { return JMM->GetNumStubSlabs(); }

#ifdef LLVM35
  virtual uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID, llvm::StringRef SectionName) { return JMM->allocateCodeSection(Size,Alignment,SectionID,SectionName); }
  virtual uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID, llvm::StringRef SectionName, bool IsReadOnly) { return JMM->allocateDataSection(Size,Alignment,SectionID,SectionName,IsReadOnly); }
#else
  virtual uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID) { return JMM->allocateCodeSection(Size,Alignment,SectionID); }
  virtual uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID, bool IsReadOnly) { return JMM->allocateDataSection(Size,Alignment,SectionID,IsReadOnly); }
#endif
  virtual void *getPointerToNamedFunction(const std::string &Name,
                                          bool AbortOnFailure = true) { return JMM->getPointerToNamedFunction(Name,AbortOnFailure); }
  virtual bool applyPermissions(std::string *ErrMsg = 0) { return JMM->applyPermissions(ErrMsg); }
  virtual void registerEHFrames(StringRef SectionData) { return JMM->registerEHFrames(SectionData); }
};

#else 
void* CALLBACK jl_getUnwindInfo(HANDLE hProcess, ULONG64 AddrBase, ULONG64 UserContext)
{
    return NULL;
}
#endif
#endif

// Code coverage

typedef std::map<std::string,std::vector<GlobalVariable*> > coveragedata_t;
static coveragedata_t coverageData;

static void coverageVisitLine(std::string filename, int line)
{
    if (filename == "" || filename == "none" || filename == "no file")
        return;
    coveragedata_t::iterator it = coverageData.find(filename);
    if (it == coverageData.end()) {
        coverageData[filename] = std::vector<GlobalVariable*>(0);
    }
    std::vector<GlobalVariable*> &vec = coverageData[filename];
    if (vec.size() <= (size_t)line)
        vec.resize(line+1, NULL);
    if (vec[line] == NULL)
        vec[line] = new GlobalVariable(*jl_Module, T_int64, false, GlobalVariable::InternalLinkage,
                                       ConstantInt::get(T_int64,0), "lcnt");
    GlobalVariable *v = vec[line];
    builder.CreateStore(builder.CreateAdd(builder.CreateLoad(v),
                                          ConstantInt::get(T_int64,1)),
                        v);
}

extern "C" void jl_write_coverage_data(void)
{
    std::string base = std::string(julia_home);
    base.erase(base.end()-7, base.end());
    base = base + "base/";
    coveragedata_t::iterator it = coverageData.begin();
    for (; it != coverageData.end(); it++) {
        std::string filename = (*it).first;
        std::vector<GlobalVariable*> &counts = (*it).second;
        if (counts.size() > 1) {
	    if (filename[0] != '/')
		filename = base + filename;
            std::ifstream inf(filename.c_str());
            if (inf.is_open()) {
		std::string outfile = filename + ".cov";
		JL_PRINTF(JL_STDOUT, "Writing %s\n", outfile.c_str());
                std::ofstream outf(outfile.c_str(), std::ofstream::trunc | std::ofstream::out);
                char line[1024];
                int l = 1;
                while (!inf.eof()) {
                    inf.getline(line, sizeof(line));
		    if (inf.fail() && !inf.bad()) {
			// Read through lines longer than 1024
			inf.clear();
			inf.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		    }
                    int count = -1;
                    if ((size_t)l < counts.size()) {
                        GlobalVariable *gv = counts[l];
                        if (gv) {
                            int *p = (int*)jl_ExecutionEngine->getPointerToGlobal(gv);
                            count = *p;
                        }
                    }
                    outf.width(9);
                    if (count == -1)
                        outf<<'-';
                    else
                        outf<<count;
                    outf.width(0);
                    outf<<" "<<line<<std::endl;
                    l++;
                }
                outf.close();
                inf.close();
            }
        }
    }
}

// Memory allocation log (malloc_log)

typedef std::map<std::string,std::vector<GlobalVariable*> > mallocdata_t;
static mallocdata_t mallocData;

static void mallocVisitLine(std::string filename, int line)
{
    if (filename == "" || filename == "none" || filename == "no file") {
	sync_gc_total_bytes();
        return;
    }
    mallocdata_t::iterator it = mallocData.find(filename);
    if (it == mallocData.end()) {
        mallocData[filename] = std::vector<GlobalVariable*>(0);
    }
    std::vector<GlobalVariable*> &vec = mallocData[filename];
    if (vec.size() <= (size_t)line)
        vec.resize(line+1, NULL);
    if (vec[line] == NULL)
        vec[line] = new GlobalVariable(*jl_Module, T_int64, false,
				       GlobalVariable::InternalLinkage,
                                       ConstantInt::get(T_int64,0), "bytecnt");
    GlobalVariable *v = vec[line];
    //builder.CreateCall2(prepare_call(show_execution_point_func), builder.CreateGlobalStringPtr(filename), ConstantInt::get(T_int32, line));
    builder.CreateStore(builder.CreateAdd(builder.CreateLoad(v, true),
                                          builder.CreateCall(prepare_call(diff_gc_total_bytes_func))),
                        v, true);
}

// Resets the malloc counts. Needed to avoid including memory usage
// from JITting.
extern "C" DLLEXPORT void jl_clear_malloc_data(void)
{
    mallocdata_t::iterator it = mallocData.begin();
    for (; it != mallocData.end(); it++) {
        std::vector<GlobalVariable*> &bytes = (*it).second;
	std::vector<GlobalVariable*>::iterator itb;
	for (itb = bytes.begin(); itb != bytes.end(); itb++) {
	    if (*itb) {
		int64_t *p = (int64_t*) jl_ExecutionEngine->getPointerToGlobal(*itb);
		*p = 0;
	    }
	}
    }
    sync_gc_total_bytes();
}

extern "C" void jl_write_malloc_log(void)
{
    mallocdata_t::iterator it = mallocData.begin();
    for (; it != mallocData.end(); it++) {
        std::string filename = (*it).first;
        std::string outfile = filename + ".mlc";
        std::vector<GlobalVariable*> &bytes = (*it).second;
        if (bytes.size() > 1) {
            std::ifstream inf(filename.c_str());
            if (inf.is_open()) {
                std::ofstream outf(outfile.c_str(), std::ofstream::trunc | std::ofstream::out);
                char line[1024];
                int l = 1;
                while (!inf.eof()) {
                    inf.getline(line, sizeof(line));
                    int nbytes = -1;
                    if ((size_t)l < bytes.size()) {
                        GlobalVariable *gv = bytes[l];
                        if (gv) {
                            int *p = (int*)jl_ExecutionEngine->getPointerToGlobal(gv);
                            nbytes = *p;
                        }
                    }
                    outf.width(9);
                    if (nbytes == -1)
                        outf<<'-';
                    else
                        outf<<nbytes;
                    outf.width(0);
                    outf<<" "<<line<<std::endl;
                    l++;
                }
                outf.close();
                inf.close();
            }
        }
    }
}

void show_execution_point(char *filename, int lno)
{
    jl_printf(JL_STDOUT, "executing file %s, line %d\n", filename, lno);
}
