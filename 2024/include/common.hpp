#ifndef COMMON_HPP
#define COMMON_HPP

#include <format>
#include <cstdio>
#include <cstdint>

template<typename... T>
void print(std::format_string<T...> fmt, T&&... args)
{
   const auto str = std::format(fmt, std::forward<T>(args)...);
   std::fputs(str.c_str(), stdout);
}

template<typename... T>
void eprint(std::format_string<T...> fmt, T&&... args)
{
   const auto str = std::format(fmt, std::forward<T>(args)...);
   std::fputs(str.c_str(), stderr);
}

template<std::size_t I>
struct constant {};

#endif // COMMON_HPP
