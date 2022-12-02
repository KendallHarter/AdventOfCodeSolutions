#include <vector>
#include <charconv>
#include <string>
#include <cstdio>
#include <cassert>
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

void day2()
{
   enum {
      o_rock = 'A',
      o_paper = 'B',
      o_scissors = 'C',
      p_rock = 'X',
      p_paper = 'Y',
      p_scissors = 'Z',
   };
   const auto calc_points1 = [](std::pair<char, char> pair) {

      const auto base_value = pair.second - p_rock + 1;
      if (pair.first == o_rock && pair.second == p_paper ||
          pair.first == o_scissors && pair.second == p_rock ||
          pair.first == o_paper && pair.second == p_scissors) {
         return 6 + base_value;
      }
      else if (pair.first - o_rock == pair.second - p_rock) {
         return 3 + base_value;
      }
      else {
         return base_value;
      }
   };
   const auto calc_points2 = [&](std::pair<char, char> pair) {
      const auto index = (pair.first - o_rock) * 3 + pair.second - 'X';
      // order for second character is lose, draw, win
      static constexpr std::pair<char, char> play_guide[] = {
         {o_rock, p_scissors}, {o_rock, p_rock}, {o_rock, p_paper},
         {o_paper, p_rock}, {o_paper, p_paper}, {o_paper, p_scissors},
         {o_scissors, p_paper}, {o_scissors, p_scissors}, {o_scissors, p_rock}
      };
      return calc_points1(play_guide[index]);
   };
   const std::vector<std::pair<char, char>> guide = []() {
      std::vector<std::pair<char, char>> to_ret;
      std::string buffer;
      while(std::getline(std::cin, buffer)) {
         if (buffer == "") {
            continue;
         }
         assert(buffer.size() == 3);
         to_ret.emplace_back(buffer[0], buffer[2]);
      }
      return to_ret;
   }();
   const auto calc_points = [&](const auto& calc_func) {
      std::vector<int> points;
      points.resize(guide.size());
      std::ranges::transform(guide, points.begin(), calc_func);
      return std::reduce(points.begin(), points.end(), 0);
   };
   print(std::format("Part 1: {}\n", calc_points(calc_points1)));
   print(std::format("Part 2: {}\n", calc_points(calc_points2)));
}

int main()
{
   using func = void(*)();
   const func days[] = {day1, day2};
   day2();
}

