#include "khparse.hpp"

#include "common.hpp"

#include <ranges>

template<typename...> struct TD;

std::pair<std::int64_t, std::int64_t> day9(const std::string& input)
{
   const auto parser = khparse::seq{
      khparse::repeat{
         khparse::seq{
            khparse::capture<"\\d">,
            khparse::capture<"\\d">
         },
         1,
         0
      },
      khparse::capture<"\\d?">
   };

   const auto res = parser.parse(input);
   if (!res) {
      eprint("Error parsing input");
   }

   const auto [raw_data, last_digit] = res.value().value;

   const std::vector<int> file_list = [&]() {
      std::vector<int> file_list;
      for (const auto [file_no, info] : std::views::enumerate(raw_data)) {
         const auto& [file_len_str, empty_len_str] = info;
         const auto file_len = file_len_str[0] - '0';
         const auto empty_len = empty_len_str[0] - '0';
         for (const auto& dummy : std::views::iota(0, file_len)) {
            (void)dummy;
            file_list.push_back(file_no);
         }
         for (const auto& dummy : std::views::iota(0, empty_len)) {
            (void)dummy;
            file_list.push_back(-1);
         }
      }
      if (!last_digit.empty()) {
         for (const auto& dummy : std::views::iota(0, last_digit[0] - '0')) {
            (void)dummy;
            file_list.push_back(std::ssize(raw_data));
         }
      }
      return file_list;
   }();

   auto part1_list = file_list;

   auto empty_iter_part1 = part1_list.begin();
   auto file_iter_part1 = part1_list.rbegin();

   while (true) {
      empty_iter_part1 = std::find(empty_iter_part1, part1_list.end(), -1);
      file_iter_part1 = std::find_if(file_iter_part1, part1_list.rend(), [](const auto& val) { return val != -1; });

      if (empty_iter_part1 >= file_iter_part1.base()) {
         break;
      }

      std::swap(*empty_iter_part1, *file_iter_part1);
      ++empty_iter_part1;
      ++file_iter_part1;
   }

   std::int64_t part1 = 0;
   for (const auto [i, val] : std::views::enumerate(part1_list | std::views::take_while([](const auto& val) { return val != -1; }))) {
      part1 += i * val;
   }

   auto part2_list = file_list;

   auto file_iter_part2 = part2_list.rbegin();

   while (true) {
      auto empty_iter_part2 = std::find(part2_list.begin(), part2_list.end(), -1);
      file_iter_part2 = std::find_if(file_iter_part2, part2_list.rend(), [](const auto& val) { return val != -1; });

      if (file_iter_part2 == part2_list.rend()) {
         break;
      }

      const auto end_file_iter = std::find_if(file_iter_part2, part2_list.rend(), [&](const auto& val) { return val != *file_iter_part2; });
      const auto file_size = std::ranges::distance(file_iter_part2, end_file_iter);

      while (empty_iter_part2 != part2_list.end()) {
         const auto end_empty_iter = std::find_if(empty_iter_part2, part2_list.end(), [&](const auto& val) { return val != -1; });
         const auto empty_size = std::ranges::distance(empty_iter_part2, end_empty_iter);

         if (empty_iter_part2 >= file_iter_part2.base()) {
            break;
         }

         if (empty_size >= file_size) {
            for (std::size_t i = 0; file_iter_part2 + i != end_file_iter; ++i) {
               std::swap(*(file_iter_part2 + i), *(empty_iter_part2 + i));
            }
            break;
         }

         empty_iter_part2 = end_empty_iter + 1;
      }

      file_iter_part2 = end_file_iter;
   }

   std::int64_t part2 = 0;
   for (const auto [i, val] : std::views::enumerate(part2_list)) {
      if (val != -1) {
         part2 += i * val;
      }
   }

   return {part1, part2};
}
