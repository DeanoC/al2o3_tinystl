cmake_minimum_required(VERSION 3.14)

set(LibName al2o3_tinystl)
project(${LibName} LANGUAGES CXX)

include(FetchContent)
FetchContent_Declare( al2o3 GIT_REPOSITORY https://github.com/DeanoC/al2o3 GIT_TAG master )
FetchContent_GetProperties(al2o3)
if(NOT al2o3_POPULATED)
	FetchContent_Populate(al2o3)
	add_subdirectory(${al2o3_SOURCE_DIR})
endif()

set(Interface
		allocator.h
		buffer.h
		hash.h
		hash_base.h
		new.h
		stddef.h
		string.h
		string_view.h
		traits.h
		unordered_map.h
		unordered_set.h
		vector.h
		)
set(Src
		)
set(Deps)

ADD_LIB(${LibName} "${Interface}" "${Src}" "${Deps}")

set( Tests
		)
set( TestDeps
		)
ADD_LIB_TESTS(${LibName} "${Interface}" "${Tests}" "${TestDeps}")
