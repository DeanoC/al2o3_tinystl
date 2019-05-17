include(FetchContent)
FetchContent_Declare( al2o3 GIT_REPOSITORY https://github.com/DeanoC/al2o3 GIT_TAG master )
FetchContent_GetProperties(al2o3)
if(NOT al2o3_POPULATED)
	FetchContent_Populate(al2o3)
	add_subdirectory(${al2o3_SOURCE_DIR} ${al2o3_BINARY_DIR})
endif()

SET_MIN_VERSIONS()
set(LibName al2o3_tinystl)
project(${LibName})


set(Interface
		allocator.hpp
		buffer.hpp
		hash.hpp
		hash_base.hpp
		new.hpp
		stddef.hpp
		string.hpp
		string_view.hpp
		traits.hpp
		unordered_map.hpp
		unordered_set.hpp
		vector.hpp
		)
set(Src
		dummy.cpp
		)
set(Deps)

ADD_LIB(${LibName} "${Interface}" "${Src}" "${Deps}")

set( Tests
		)
set( TestDeps
		)
ADD_LIB_TESTS(${LibName} "${Interface}" "${Tests}" "${TestDeps}")
