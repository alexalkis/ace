#include <iostream>
#include <string>
#include <stdlib.h>

#define COMPILER        "./ace"
#define ASSEMBLER       "vasmm68k_mot"
#define LINKER          "vlink"

int main(int argc, char **argv)
{
    int i;
    std::string compline;

    if (argc<2) {
        fprintf(stderr, "Usage: %s [-O] file.b\n",argv[0]);
        return 0;
    }
    compline="";
    for(i=1; i<argc; ++i) {
        compline += argv[i];
        compline += " ";
    }
    std::string filename=argv[argc-1];
    size_t dot = filename.rfind('.');
    if (dot==std::string::npos) {
        fprintf(stderr, "%s error: Filename should end in .b[as]\n",argv[0]);
        return 10;
    }
    std::string ext = filename.substr(dot);
    std::string basename = filename.substr(0,dot);
    std::string compiler = std::string(COMPILER) + " " + compline;
    std::string assembler = std::string(ASSEMBLER) + "  -Fhunk -o " + basename + ".o " + basename +".s";

    char *lp = getenv("ACE_Basic");
    std::string libpath;
    if (lp) {
        libpath = lp;
        libpath+= "lib/";
    } else {
        fprintf(stderr,"\n\n\nPlease define ACE_Basic to show the path where ACE's bmaps are...\n"
                       "(i.e. export ACE_Basic=\"/boo/foo/\" No need to escape spaces)\n"
        );
        return 20;
    }
    if (libpath.empty()) {
        fprintf(stderr,"Please define ACElib to show the path where ACE's lib folder is...\n(i.e. export ACElib=/foo/boo/)\n");
        exit(20);
    }
    std::string linker = std::string(LINKER) +" -bamigahunk -o "+basename+ " " + basename+".o "
            + libpath + "ami.lib " + libpath + "db.lib " + libpath + "startup.lib";

    std::cout << compiler << "\n"
            << assembler << "\n"
            << linker << "\n";


    int rc=system(compiler.c_str());
    if (rc!=0) return rc;

    rc = system(assembler.c_str());
    printf("assembler rc=%d\n",rc);
    if (rc!=0) return rc;

    rc = system(linker.c_str());

    printf("linker rc=%d\n",rc);
    return rc;
}
