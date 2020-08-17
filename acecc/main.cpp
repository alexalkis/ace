#include <iostream>
#include <string>
#include <stdlib.h>

#define COMPILER        "ace"
#define ASSEMBLER       "vasmm68k_mot"
#define LINKER          "vlink"

int main(int argc, char **argv)
{
    int i;
    std::string compline;

    if (argc<2) {
        std::cerr << "Usage: " << argv[0] << " [-O] file.b\n";
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
        std::cerr << argv[0] << " error: Filename should end in .b[as]\n";
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
        std::cerr << "\n\n\nPlease define ACE_Basic to show the path where ACE's bmaps are...\n"
                     "(i.e. export ACE_Basic=\"/boo/foo/\" No need to escape spaces)\n";
        return 20;
    }
    std::string linker = std::string(LINKER) +" -bamigahunk -o "+basename+ " " + basename+".o "
            + libpath + "ami.lib " + libpath + "db.lib " + libpath + "startup.lib";

    std::cout << compiler << "\n"
            << assembler << "\n"
            << linker << "\n";


    int rc=system(compiler.c_str());
    if (rc!=0) {
        std::cerr << "Compiler returned " << rc << "\n";
        return rc;
    }

    rc = system(assembler.c_str());
    if (rc!=0) {
        std::cerr << "Assembler returned " << rc << "\n";
        return rc;
    }

    rc = system(linker.c_str());
    if (rc!=0) {
        std::cerr << "Linker returned " << rc << "\n";
        return rc;
    }
    return rc;
}
