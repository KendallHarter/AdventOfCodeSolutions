cmake_minimum_required(VERSION 3.10)

project(aoc_2024)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_CXX_EXTENSIONS OFF)

function(clone_repo name url tag)
   execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/repos/${name})
   execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/repo_versions/)
   if (EXISTS ${CMAKE_CURRENT_BINARY_DIR}/repo_versions/${name})
      file(READ ${CMAKE_CURRENT_BINARY_DIR}/repo_versions/${name} version)
      if (${version} STREQUAL ${tag})
         return()
      endif()
   endif()
   find_package(Git)
   # gcmd = git command
   function(gcmd)
      execute_process(
         COMMAND ${GIT_EXECUTABLE} ${ARGV}
         WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/repos/${name}
      )
   endfunction()

   gcmd(init)
   gcmd(remote add origin ${url})
   gcmd(fetch --depth 1 origin ${tag})
   gcmd(checkout FETCH_HEAD)
   file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/repo_versions/${name} ${tag})
endfunction()

clone_repo(khparse git@github.com:KendallHarter/khparse.git main)

add_subdirectory(${CMAKE_CURRENT_BINARY_DIR}/repos/khparse)

add_executable(solution
   days/1.cpp
   days/2.cpp
   days/3.cpp
   days/4.cpp
   days/5.cpp
   days/6.cpp
   days/7.cpp
   main.cpp
)
target_link_libraries(solution khparse::khparse)
target_include_directories(solution PRIVATE include)

add_custom_target(create_input_symlink
   COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_CURRENT_SOURCE_DIR}/input" "${CMAKE_CURRENT_BINARY_DIR}/input"
)
