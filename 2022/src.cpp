#include <vector>
#include <charconv>
#include <string>
#include <cstdio>
#include <iostream>
#include <algorithm>
#include <format>
#include <numeric>

void print(const std::string& s)
{
   std::fputs(s.c_str(), stdout);
}

void eprint(const std::string& s)
{
   std::fputs(s.c_str(), stderr);
}

void day1()
{
   const std::vector<std::vector<int>> calorie_list = []() {
      std::vector<std::vector<int>> list;
      list.push_back({});
      std::string buffer;
      while(std::getline(std::cin, buffer)) {
         if (buffer == "") {
            list.push_back({});
            continue;
         }
         int value;
         const auto [ptr, ec] = std::from_chars(buffer.data(), buffer.data() + buffer.size(), value);
         if (ec != std::errc{}) {
            eprint(std::format("Error parsing \"{}\"\n", buffer));
            std::exit(1);
         }
         list.back().push_back(value);
      }
      return list;
   }();
   const std::vector<int> values = [&]() {
      std::vector<int> to_ret;
      to_ret.resize(calorie_list.size());
      std::ranges::transform(calorie_list, to_ret.begin(), [](const auto& v) { return std::reduce(v.begin(), v.end(), 0); });
      std::ranges::partial_sort(to_ret, to_ret.begin() + 3, std::ranges::greater{});
      return to_ret;
   }();
   print(std::format("Part 1: {}\n", values.front()));
   print(std::format("Part 2: {}\n", std::reduce(values.begin(), values.begin() + 3, 0)));
}

int main()
{
   using func = void(*)();
   const func days[] = {day1};
   day1();
}

