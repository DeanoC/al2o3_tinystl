set(LibName al2o3_tinystl)
project(${LibName})

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
