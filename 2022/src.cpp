#include <vector>
#include <charconv>
#include <string>
#include <cstdio>
#include <cassert>
#include <iostream>
#include <algorithm>
#include <format>
#include <numeric>
#include <concepts>
#include <set>

void print(const std::string& s)
{
   std::fputs(s.c_str(), stdout);
}

void eprint(const std::string& s)
{
   std::fputs(s.c_str(), stderr);
}

template<std::integral T>
T to_int(std::string_view v)
{
   T value;
   const auto [ptr, ec] = std::from_chars(v.begin(), v.end(), value);
   if (ec != std::errc{}) {
      eprint(std::format("Error parsing \"{}\"\n", v));
      std::exit(1);
   }
   return value;
}

template<typename... T>
std::set<T...> merge_sets(std::set<T...> s1, std::set<T...> s2)
{
   s1.merge(s2);
   return s1;
}

void day1()
{
   const std::vector<std::vector<int>> calorie_list = []() {
      std::vector<std::vector<int>> list;
      list.push_back({});
      std::string buffer;
      while (std::getline(std::cin, buffer)) {
         if (buffer.empty()) {
            list.push_back({});
         }
         else {
            list.back().push_back(to_int<int>(buffer));
         }
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
      if ((pair.first == o_rock && pair.second == p_paper) ||
          (pair.first == o_scissors && pair.second == p_rock) ||
          (pair.first == o_paper && pair.second == p_scissors)) {
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
      while (std::getline(std::cin, buffer)) {
         if (buffer.empty()) {
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

void day3()
{
   const auto calc_weight = [](char c) {
      if (c >= 'a' && c <= 'z') {
         return c - 'a' + 1;
      }
      else {
         return c - 'A' + 27;
      }
   };
   const std::vector<std::pair<std::set<char>, std::set<char>>> rucksacks = []() {
      std::vector<std::pair<std::set<char>, std::set<char>>> to_ret;
      std::string buffer;
      while (std::getline(std::cin, buffer)) {
         assert(buffer.size() % 2 == 0);
         const auto first_half = buffer.substr(0, buffer.size() / 2);
         const auto second_half = buffer.substr(buffer.size() / 2);
         to_ret.push_back({{first_half.begin(), first_half.end()}, {second_half.begin(), second_half.end()}});
      }
      return to_ret;
   }();
   int total_weight1 = 0;
   for (const auto& [s1, s2] : rucksacks) {
      std::vector<char> shared_chars;
      std::ranges::set_intersection(s1, s2, std::back_inserter(shared_chars));
      for (const auto& c : shared_chars) {
         total_weight1 += calc_weight(c);
      }
   }
   print(std::format("Part 1: {}\n", total_weight1));

   int total_weight2 = 0;
   for (std::size_t i = 0; i < rucksacks.size() / 3; ++i) {
      const auto s1 = merge_sets(rucksacks[i * 3 + 0].first, rucksacks[i * 3 + 0].second);
      const auto s2 = merge_sets(rucksacks[i * 3 + 1].first, rucksacks[i * 3 + 1].second);
      const auto s3 = merge_sets(rucksacks[i * 3 + 2].first, rucksacks[i * 3 + 2].second);
      std::vector<char> shared_chars;
      std::ranges::set_intersection(s1, s2, std::back_inserter(shared_chars));
      std::set<char> temp_set(shared_chars.begin(), shared_chars.end());
      std::vector<char> answer;
      std::ranges::set_intersection(temp_set, s3, std::back_inserter(answer));
      for (const auto& c : answer) {
         total_weight2 += calc_weight(c);
      }
   }
   print(std::format("Part 2: {}\n", total_weight2));
}

void day4()
{
   const std::vector<std::pair<std::pair<int, int>, std::pair<int, int>>> ranges = []() {
      const auto make_range = [](const std::string_view v) -> std::pair<int, int> {
         const auto split_loc = v.find('-');
         const auto first = v.substr(0, split_loc);
         const auto second = v.substr(split_loc + 1);
         return {to_int<int>(first), to_int<int>(second)};
      };
      std::vector<std::pair<std::pair<int, int>, std::pair<int, int>>> to_ret;
      std::string buffer;
      while (std::getline(std::cin, buffer)) {
         if (buffer.empty()) {
            continue;
         }
         const auto split_loc = buffer.find(',');
         const auto first = buffer.substr(0, split_loc);
         const auto second = buffer.substr(split_loc + 1);
         to_ret.emplace_back(make_range(first), make_range(second));
      }
      return to_ret;
   }();
   const auto is_subrange = [](const std::pair<std::pair<int, int>, std::pair<int, int>>& p) {
      return (p.first.first <= p.second.first && p.first.second >= p.second.second) 
         ||  (p.first.first >= p.second.first && p.first.second <= p.second.second);
   };
   // const auto subranges = ranges | std::ranges::filter_view(is_subrange);
   // const auto num_subranges = std::distance(subranges.begin(), subranges.end());
   auto num_subrange = 0;
   for (const auto& p : ranges) {
      num_subrange += is_subrange(p);
   }
   print(std::format("Part 1: {}\n", num_subrange));
   const auto is_overlaping = [](const std::pair<std::pair<int, int>, std::pair<int, int>>& p) {
      return (p.first.first >= p.second.first && p.first.first <= p.second.second)
         ||  (p.first.second >= p.second.first && p.first.second <= p.second.second)
         ||  (p.second.first >= p.first.first && p.second.first <= p.first.second)
         ||  (p.second.second >= p.first.first && p.second.second <= p.first.second);
   };
   auto num_overlaping = 0;
   for (const auto& p : ranges) {
      num_overlaping += is_overlaping(p);
   }
   print(std::format("Part 2: {}\n", num_overlaping));
}

int main(int argc, const char* argv[])
{
   using func = void(*)();
   constexpr func days[] = {day1, day2, day3, day4};
   constexpr int num_days = sizeof(days) / sizeof(func);
   if (argc != 2) {
      eprint(std::format("Usage: {} day", argv[0]));
   }
   const auto day = to_int<int>(argv[1]);
   if (day < 1 || day > num_days) {
      eprint(std::format("Invalid day {}", day));
   }
   days[day - 1]();
}

