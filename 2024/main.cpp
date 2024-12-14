#include "common.hpp"

#include <fstream>

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

std::pair<std::int64_t, std::int64_t> day1(const std::string& input);
std::pair<std::int64_t, std::int64_t> day2(const std::string& input);
std::pair<std::int64_t, std::int64_t> day3(const std::string& input);
std::pair<std::int64_t, std::int64_t> day4(const std::string& input);
std::pair<std::int64_t, std::int64_t> day5(const std::string& input);
std::pair<std::int64_t, std::int64_t> day6(const std::string& input);
std::pair<std::int64_t, std::int64_t> day7(const std::string& input);
std::pair<std::int64_t, std::int64_t> day8(const std::string& input);
std::pair<std::int64_t, std::int64_t> day9(const std::string& input);
std::pair<std::int64_t, std::int64_t> day10(const std::string& input);
std::pair<std::int64_t, std::int64_t> day11(const std::string& input);
std::pair<std::int64_t, std::int64_t> day12(const std::string& input);
std::pair<std::int64_t, std::int64_t> day13(const std::string& input);
std::pair<std::int64_t, std::int64_t> day14(const std::string& input);

int main(int argc, const char* argv[])
{
   using func = std::pair<std::int64_t, std::int64_t> (*)(const std::string&);
   constexpr func days[] = {
      day1, day2, day3, day4, day5, day6, day7, day8, day9, day10,
      day11, day12, day13, day14
   };
   constexpr int num_days = std::ssize(days);
   if (argc != 2) {
      eprint("Usage: {} day\n", argv[0]);
      std::exit(1);
   }
   const auto day = to_int<int>(argv[1]);
   if (day < 1 || day > num_days) {
      eprint("Invalid day {}\n", day);
      std::exit(1);
   }
   const auto file_name = std::format("input/day{}.txt", day);
   const auto [part1, part2] = days[day - 1](read_file(file_name.c_str()));

   print("Day {}\nPart 1: {}\nPart 2: {}\n", day, part1, part2);
}
