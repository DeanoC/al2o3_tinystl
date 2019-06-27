#pragma once
#ifndef AL2O3_TINYSTL_ALGORITHM_H_
#define AL2O3_TINYSTL_ALGORITHM_H_

#include "al2o3_tinystl/pdqsort.hpp"

namespace tinystl {

template<class Iter, class Compare>
inline void sort(Iter begin, Iter end, Compare comp) {
	tinystl::pdqsort(begin, end, comp);
}

template<class Iter>
inline void sort(Iter begin, Iter end) {
	typedef typename std::iterator_traits<Iter>::value_type T;
	tinystl::pdqsort(begin, end, std::less<T>());
}

} // end namesapce
#endif