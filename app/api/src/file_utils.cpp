#include <string>
#include <fstream>

#include "api/file_utils.h"
#include "api/logger.h"

namespace SonicPi
{

std::string file_read(const fs::path& fileName)
{
    std::ifstream in(fileName, std::ios::in | std::ios::binary);
    if (in)
    {
        std::string contents;
        in.seekg(0, std::ios::end);
        contents.resize(size_t(in.tellg()));
        in.seekg(0, std::ios::beg);
        in.read(&contents[0], contents.size());
        in.close();
        return (contents);
    }
    else
    {
        LOG(ERR, "File Not Found: " << fileName.string());
    }
    return std::string();
}

} // namespace SonicPi
