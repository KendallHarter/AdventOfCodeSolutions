#include "khparse.hpp"

#include <format>
#include <string_view>
#include <fstream>
#include <ranges>
#include <unordered_map>

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

void day1(const std::string& input)
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
      eprint("Parse error!\n");
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
   print("Part 1: {}\n", part1);

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
   print("Part 2: {}\n", part2);
}

void day2(const std::string& input)
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
      eprint("Parse error!\n");
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
   print("Part 1: {}\n", part1);

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
   print("Part 2: {}\n", part2);
}

int main(int argc, const char** argv)
{
   using func = void(*)(const std::string&);
   constexpr func days[] = {day1, day2};
   constexpr int num_days = std::ssize(days);
   if (argc != 2) {
      eprint("Usage: {} day\n", argv[0]);
   }
   const auto day = to_int<int>(argv[1]);
   if (day < 1 || day > num_days) {
      eprint("Invalid day {}\n", day);
   }
   const auto file_name = std::format("input/day{}.txt", day);
   days[day - 1](read_file(file_name.c_str()));
}
