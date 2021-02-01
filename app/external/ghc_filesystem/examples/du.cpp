#include <iostream>
#include <iomanip>
#include <chrono>

#if defined(__cplusplus) && __cplusplus >= 201703L && defined(__has_include)
#if __has_include(<filesystem>)
#define GHC_USE_STD_FS
#include <filesystem>
namespace fs = std::filesystem;
#endif
#endif
#ifndef GHC_USE_STD_FS
#include <ghc/filesystem.hpp>
namespace fs = ghc::filesystem;
#endif

int main(int argc, char* argv[])
{
#ifdef GHC_FILESYSTEM_VERSION
    fs::u8arguments u8guard(argc, argv);
    if(!u8guard.valid()) {
        std::cerr << "Invalid character encoding, UTF-8 based encoding needed." << std::endl;
        std::exit(EXIT_FAILURE);
    }
#endif
    if(argc > 2) {
        std::cerr << "USAGE: du <path>" << std::endl;
        exit(1);
    }
    fs::path dir{"."};
    if(argc == 2) {
        dir = fs::u8path(argv[1]);
    }

    uint64_t totalSize = 0;
    int totalDirs = 0;
    int totalFiles = 0;
    int maxDepth = 0;
    
    try {
        auto rdi = fs::recursive_directory_iterator(dir);
        for(auto de : rdi) {
            if(rdi.depth() > maxDepth) {
                maxDepth = rdi.depth();
            }
            if(de.is_regular_file()) {
                totalSize += de.file_size();
                ++totalFiles;
            }
            else if(de.is_directory()) {
                ++totalDirs;
            }
        }
    }
    catch(fs::filesystem_error fe) {
        std::cerr << "Error: " << fe.what() << std::endl;
        exit(1);
    }
    std::cout << totalSize << " bytes in " << totalFiles << " files and " << totalDirs << " directories, maximum depth: " << maxDepth << std::endl;
    return 0;
}
