#include "khparse.hpp"

#include "common.hpp"

#include <ranges>

enum struct op : std::int8_t {
   add,
   mult,
   append
};

bool increase_op_list(std::vector<op>& ops)
{
   for (std::size_t i = 0; i < ops.size(); ++i) {
      if (ops[i] != op::append) {
         ops[i] = static_cast<op>(std::to_underlying(ops[i]) + 1);
         return true;
      }
      else {
         ops[i] = op::add;
      }
   }
   return false;
}

std::pair<std::int64_t, std::int64_t> day7(const std::string& input)
{
   const auto parser = khparse::repeat{
      khparse::seq{
         khparse::i64,
         khparse::drop<": ">,
         khparse::repeat{
            khparse::with_skipper,
            khparse::drop<" ">,
            khparse::i64
         },
         khparse::drop<"\\n">
      }
   };

   const auto res = parser.parse(input);
   if (!res) {
      eprint("Parse failure\n");
      std::exit(1);
   }

   const auto values = res.value().value;

   const auto is_valid_part_1 = [](const auto& val) {
      const auto& [result, inputs] = val;
      const auto end_val = (1 << (inputs.size() - 1));
      for (std::uint64_t ops = 0; ops != end_val; ++ops) {
         std::uint64_t total = inputs[0];
         for (std::size_t i = 1; i < inputs.size(); ++i) {
            const auto bit_mask = (1 << (i - 1));
            if (ops & bit_mask) {
               total += inputs[i];
            }
            else {
               total *= inputs[i];
            }
         }
         if (total == result) {
            return true;
         }
      }
      return false;
   };

   const auto part1 = std::ranges::fold_left(
      values | std::views::filter(is_valid_part_1),
      0ll,
      [](std::int64_t sum, const auto& val) {
         return sum + std::get<0>(val);
      }
   );

   const auto is_valid_part_2 = [](const auto& val) {
      const auto& [result, inputs] = val;
      std::vector<op> ops(inputs.size() - 1, op::add);
      do {
         std::uint64_t total = inputs[0];
         for (std::size_t i = 1; i < inputs.size(); ++i) {
            switch (ops[i - 1]) {
               case op::add: total += inputs[i]; break;
               case op::mult: total *= inputs[i]; break;
               case op::append: {
                  total = std::stoll(std::format("{}{}", total, inputs[i]));
                  break;
               }
            }
         }
         if (total == result) {
            return true;
         }
      } while(increase_op_list(ops));
      return false;
   };

   const auto part2 = std::ranges::fold_left(
      values | std::views::filter(is_valid_part_2),
      0ll,
      [](std::int64_t sum, const auto& val) {
         return sum + std::get<0>(val);
      }
   );

   return {part1, part2};
}
