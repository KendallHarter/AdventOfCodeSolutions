#include "khparse.hpp"

#include <format>
#include <string_view>
#include <fstream>
#include <ranges>
#include <unordered_map>
#include <unordered_set>
#include <numeric>

template<std::size_t I>
struct constant {};

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

template<std::integral T>
T to_int(std::string_view v) noexcept
{
   T value;
   const auto [ptr, ec] = std::from_chars(v.begin(), v.end(), value);
   if (ec != std::errc{}) {
      eprint("Error parsing \"{}\"\n", v);
      std::exit(1);
   }
   return value;
}

std::string read_file(const char* file_name)
{
   std::ifstream fin(file_name);
   if (!fin) {
      eprint("Could not open file {}\n", file_name);
      std::exit(1);
   }
   fin.seekg(0, std::ios::end);
   const auto size = fin.tellg();
   fin.seekg(0, std::ios::beg);
   std::string to_ret(size, ' ');
   fin.read(to_ret.data(), size);
   return to_ret;
}

std::pair<std::int64_t, std::int64_t> day1(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::seq{
         khparse::with_skipper,
         khparse::drop<"\\s">,
         khparse::i64,
         khparse::i64
      },
      1,
      0
   };
   const auto result = parser.parse(input);
   if (!result.has_value()) {
      eprint("Parse error\n");
      std::exit(1);
   }
   const auto values = result.value().value;
   const auto sort_list = [&]<std::size_t I>(constant<I>) {
      auto r = values | std::views::elements<I>;
      std::vector<std::int64_t> to_ret(r.begin(), r.end());
      std::ranges::sort(to_ret);
      return to_ret;
   };
   const auto list1 = sort_list(constant<0>{});
   const auto list2 = sort_list(constant<1>{});
   const auto part1 = std::ranges::fold_left(
      std::views::zip(list1, list2),
      0ll,
      [](std::int64_t total, const std::pair<std::int64_t, std::int64_t>& vals) noexcept -> std::int64_t {
         return total + std::abs(vals.first - vals.second);
      }
   );

   std::unordered_map<std::int64_t, std::int64_t> number_counts;
   for (const auto& val : list2) {
      number_counts[val] += 1;
   }
   const auto part2 = std::ranges::fold_left(
      list1,
      0ll,
      [&](std::int64_t total, std::int64_t val) noexcept -> int {
         return total + val * number_counts[val];
      }
   );

   return {part1, part2};
}

std::pair<std::int64_t, std::int64_t> day2(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::seq{
         khparse::repeat{
            khparse::seq{
               khparse::with_skipper,
               khparse::drop<" ">,
               khparse::i64
            }
         },
         khparse::drop<"\\n">
      },
      1,
      0
   };
   const auto result = parser.parse(input);
   if (!result.has_value()) {
      eprint("Parse error\n");
      std::exit(1);
   }
   const auto diff_is_safe = [](bool is_increasing, std::int64_t val1, std::int64_t val2) noexcept -> bool {
      const auto diff = static_cast<int>(std::abs(val1 - val2));
      if (diff == 0 || diff > 3) {
         return false;
      }
      if (is_increasing && val1 > val2) {
         return false;
      }
      if (!is_increasing && val1 < val2) {
         return false;
      }
      return true;
   };

   const auto line_is_safe = [&](const auto& line) noexcept {
      if (line.size() < 2 || line[0] == line[1]) {
         return false;
      }
      const bool is_increasing = line[0] < line[1];
      for (const auto& [val1, val2] : std::views::zip(line, line | std::views::drop(1))) {
         if (!diff_is_safe(is_increasing, val1, val2)) {
            return false;
         }
      }
      return true;
   };

   int part1 = 0;
   for (const auto& line : result.value().value) {
      if (line_is_safe(line)) {
         part1 += 1;
      }
   }

   int part2 = 0;
   for (const auto& line : result.value().value) {
      const bool is_safe = [&]() {
         if (line_is_safe(line)) {
            return true;
         }
         for (std::size_t i = 0; i < line.size(); ++i) {
            // No concat view :c
            // so do this instead
            auto line_copy = line;
            line_copy.erase(line_copy.begin() + i);
            if (line_is_safe(line_copy)) {
               return true;
            }
         }
         return false;
      }();
      if (is_safe) {
         part2 += 1;
      }
   }

   return {part1, part2};
}

std::pair<std::int64_t, std::int64_t> day3(const std::string& input)
{
   const auto mul_parser = khparse::seq{
      khparse::drop<"mul\\(">,
      khparse::i64,
      khparse::drop<",">,
      khparse::i64,
      khparse::drop<"\\)">
   };
   const auto do_parser = khparse::drop<"do\\(\\)">;
   const auto do_not_parser = khparse::drop<"don't\\(\\)">;
   std::int64_t part1 = 0;
   for (const char* str = input.c_str(); str != input.c_str() + input.size();) {
      const auto res = mul_parser.parse(std::string_view{str, input.c_str() + input.size()});
      if (res) {
         const auto [val1, val2] = res.value().value;
         part1 += val1 * val2;
         str = res.value().rest;
      }
      else {
         ++str;
      }
   }

   std::int64_t part2 = 0;
   bool enabled = true;
   for (const char* str = input.c_str(); str != input.c_str() + input.size();) {
      const auto sub_input = std::string_view{str, input.c_str() + input.size()};
      if (enabled) {
         if (const auto res = do_not_parser.parse(sub_input)) {
            enabled = false;
            str = res.value().rest;
         }
         else if (const auto res = mul_parser.parse(sub_input)) {
            const auto [val1, val2] = res.value().value;
            part2 += val1 * val2;
            str = res.value().rest;
         }
         else {
            ++str;
         }
      }
      else {
         if (const auto res = do_parser.parse(sub_input)) {
            enabled = true;
            str = res.value().rest;
         }
         else {
            ++str;
         }
      }
   }

   return {part1, part2};
}

std::pair<std::int64_t, std::int64_t> day4(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::seq{
         khparse::capture<"[^\\n]+">,
         khparse::drop<"\\n">
      }
   };
   const auto res = parser.parse(input);
   if (!res) {
      eprint("Parse error\n");
      std::exit(1);
   }
   const auto grid = res.value().value;
   int part1 = 0;
   for (int y = 0; y < grid.size(); ++y) {
      for (int x = 0; x < grid[y].size(); ++x) {
         // forward
         if (grid[y].substr(x, 4) == "XMAS") {
            part1 += 1;
         }
         // forward backwards
         if (grid[y].substr(x, 4) == "SAMX") {
            part1 += 1;
         }
         if (y + 3 < grid.size()) {
            // down
            if (grid[y][x] == 'X' && grid[y + 1][x] == 'M' && grid[y + 2][x] == 'A' && grid[y + 3][x] == 'S') {
               part1 += 1;
            }
            // down backwards
            if (grid[y][x] == 'S' && grid[y + 1][x] == 'A' && grid[y + 2][x] == 'M' && grid[y + 3][x] == 'X') {
               part1 += 1;
            }

            if (x + 3 < grid[y].size()) {
               // diagonal right-down
               if (grid[y][x] == 'X' && grid[y + 1][x + 1] == 'M' && grid[y + 2][x + 2] == 'A' && grid[y + 3][x + 3] == 'S') {
                  part1 += 1;
               }
               // diagonal right-down backwards
               if (grid[y][x] == 'S' && grid[y + 1][x + 1] == 'A' && grid[y + 2][x + 2] == 'M' && grid[y + 3][x + 3] == 'X') {
                  part1 += 1;
               }
            }

            if (x - 3 >= 0) {
               // diagonal left-down
               if (grid[y][x] == 'X' && grid[y + 1][x - 1] == 'M' && grid[y + 2][x - 2] == 'A' && grid[y + 3][x - 3] == 'S') {
                  part1 += 1;
               }
               // diagonal left-down backwards
               if (grid[y][x] == 'S' && grid[y + 1][x - 1] == 'A' && grid[y + 2][x - 2] == 'M' && grid[y + 3][x - 3] == 'X') {
                  part1 += 1;
               }
            }
         }
      }
   }

   int part2 = 0;
   for (int y = 1; y < grid.size() - 1; ++y) {
      for (int x = 1; x < grid[y].size() - 1; ++x) {
         if (grid[y][x] == 'A') {
            const auto upper_left = grid[y - 1][x - 1];
            const auto upper_right = grid[y - 1][x + 1];
            const auto lower_left = grid[y + 1][x - 1];
            const auto lower_right = grid[y + 1][x + 1];

            if (upper_left == 'M' && upper_right == 'S' && lower_left == 'M' && lower_right == 'S') {
               part2 += 1;
            }
            else if (upper_left == 'S' && upper_right == 'S' && lower_left == 'M' && lower_right == 'M') {
               part2 += 1;
            }
            else if (upper_left == 'M' && upper_right == 'M' && lower_left == 'S' && lower_right == 'S') {
               part2 += 1;
            }
            else if (upper_left == 'S' && upper_right == 'M' && lower_left == 'S' && lower_right == 'M') {
               part2 += 1;
            }
         }
      }
   }

   return {part1, part2};
}

#include <iostream>

std::pair<std::int64_t, std::int64_t> day5(const std::string& input)
{
   const auto parser = khparse::seq{
      khparse::repeat{
         khparse::seq{
            khparse::i64, khparse::drop<"\\|">, khparse::i64, khparse::drop<"\\n">
         }
      },
      khparse::drop<"\\n">,
      khparse::repeat{
         khparse::seq{
            khparse::repeat{
               khparse::with_skipper,
               khparse::drop<",">,
               khparse::i64,
               1,
               0
            },
            khparse::drop<"\\n">
         }
      }
   };

   const auto res = parser.parse(input);
   if (!res) {
      eprint("Parse error\n");
      std::exit(1);
   }
   const auto& [page_requirements, page_lists] = res.value().value;

   std::unordered_map<std::int64_t, std::unordered_set<std::int64_t>> page_rules;
   for (const auto& [before_page, after_page] : page_requirements) {
      page_rules[before_page].insert(after_page);
   }

   const auto is_valid = [&](const std::vector<std::int64_t>& line) {
      for (std::size_t i = 1; i < line.size(); ++i) {
         const auto current = line[i];
         for (const auto& before : line | std::views::take(i)) {
            if (page_rules[current].count(before) > 0) {
               return false;
            }
         }
      }
      return true;
   };

   int part1 = 0;
   for (const auto& line : page_lists) {
      if (is_valid(line)) {
         part1 += *std::midpoint(line.data(), line.data() + line.size());
      }
   }

   int part2 = 0;

   for (const auto& line : page_lists) {
      if (!is_valid(line)) {
         const auto fixed_line = [&]() -> std::vector<std::int64_t> {
            auto line_copy = line;
            while (!is_valid(line_copy)) {
               for (std::size_t i = 0; i < line_copy.size(); ++i) {
                  for (std::size_t j = i + 1; j < line_copy.size(); ++j) {
                     if (page_rules[line_copy[i]].count(line_copy[j]) == 0) {
                        std::swap(line_copy[i], line_copy[j]);
                     }
                  }
               }
            }
            return line_copy;
         }();
         part2 += *std::midpoint(fixed_line.data(), fixed_line.data() + fixed_line.size());
      }
   }

   return {part1, part2};
}

int main(int argc, const char* argv[])
{
   using func = std::pair<std::int64_t, std::int64_t> (*)(const std::string&);
   constexpr func days[] = {day1, day2, day3, day4, day5};
   constexpr int num_days = std::ssize(days);
   if (argc != 2) {
      eprint("Usage: {} day\n", argv[0]);
   }
   const auto day = to_int<int>(argv[1]);
   if (day < 1 || day > num_days) {
      eprint("Invalid day {}\n", day);
   }
   const auto file_name = std::format("input/day{}.txt", day);
   const auto [part1, part2] = days[day - 1](read_file(file_name.c_str()));

   print("Day {}\nPart 1: {}\nPart 2: {}\n", day, part1, part2);
}
