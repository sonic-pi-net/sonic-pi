# Stuff that might be useful in future
macro(print_all_variables)
    message(STATUS "print_all_variables------------------------------------------{")
    get_cmake_property(_variableNames VARIABLES)
    foreach (_variableName ${_variableNames})
        message(STATUS "${_variableName}=${${_variableName}}")
    endforeach()
    message(STATUS "print_all_variables------------------------------------------}")
endmacro()

# Function:                 EXCLUDE_FILES_FROM_DIR_IN_LIST
# Description:              Exclude all files from a list under a specific directory.
# Param _InFileList:        Input and returned List 
# Param _excludeDirName:    Name of the directory, which shall be ignored.
# Param _verbose:           Print the names of the files handled
function (exclude_files_from_dir_in_list _InFileList _excludeDirName _verbose)
  foreach (ITR ${_InFileList})
    if ("${_verbose}")
      message(STATUS "ITR=${ITR}")
    endif ("${_verbose}")

    if ("${ITR}" MATCHES "(.*)${_excludeDirName}(.*)")                   # Check if the item matches the directory name in _excludeDirName
      message(STATUS "Remove Item from List:${ITR}")
      list (REMOVE_ITEM _InFileList ${ITR})                              # Remove the item from the list
    endif ("${ITR}" MATCHES "(.*)${_excludeDirName}(.*)")

  endforeach(ITR)
  set(SOURCE_FILES ${_InFileList} PARENT_SCOPE)                          # Return the SOURCE_FILES variable to the calling parent
endfunction (exclude_files_from_dir_in_list)

macro(copy_existing_files TARGET_PROJECT GLOBPAT DESTINATION)
  file(GLOB COPY_FILES
    ${GLOBPAT})
  message(STATUS "Globbed: " ${COPY_FILES})
  foreach(FILENAME ${COPY_FILES})
    set(SRC "${FILENAME}")
    set(DST "${DESTINATION}")

    MESSAGE(STATUS "Post copy existing file: " ${SRC} ", " ${DST})
    add_custom_command(
      TARGET ${TARGET_PROJECT} POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy ${SRC} ${DST}
      )
  endforeach(FILENAME)
endmacro(copy_existing_files)

macro(copy_generated_file TARGET_PROJECT SRC DST)
    MESSAGE(STATUS "Post copy file: " ${SRC} ", " ${DST})
    add_custom_command(
      TARGET ${TARGET_PROJECT} POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy ${SRC} ${DST}
      )
endmacro(copy_generated_file)
