use aiken/builtin

pub const master_token_name = "NightProtocolParams"

pub fn intersect_length(list1: List<ByteArray>, list2: List<ByteArray>) -> Int {
  when list1 is {
    [] -> 0
    [bytes1, ..rest1] ->
      when list2 is {
        [] -> 0
        [bytes2, ..rest2] ->
          if bytes1 == bytes2 {
            1 + intersect_length(rest1, rest2)
          } else if builtin.less_than_bytearray(bytes1, bytes2) {
            intersect_length(rest1, list2)
          } else {
            intersect_length(list1, rest2)
          }
      }
  }
}

test intersect_length_1() {
  2 == intersect_length([#"aa", #"bb", #"cc"], [#"aaaa", #"bb", #"cc"])
}

test intersect_length_2() {
  2 == intersect_length([#"aa", #"bbbb", #"cc"], [#"aa", #"bb", #"cc"])
}

test intersect_length_3() {
  2 == intersect_length([#"aa", #"cc", #"bbbb"], [#"aa", #"bb", #"cc"])
}

test intersect_length_4() {
  0 == intersect_length([#"aa", #"bb", #"cc"], [#"aaaa", #"bbbb", #"cccc"])
}

test intersect_length_5() {
  3 == intersect_length([#"aa", #"bb", #"cc"], [#"aa", #"bb", #"cc"])
}

pub fn must_find(haystack: List<a>, predicate: fn(a) -> Bool) -> a {
  expect [item, ..rest] = haystack

  if predicate(item) {
    item
  } else {
    must_find(rest, predicate)
  }
}

test must_find_1() fail {
  expect _ = must_find([], fn(_input) { True })
}

test must_find_2() {
  must_find([1, 2, 3], fn(input) { input == 2 }) == 2
}

test must_find_3() fail {
  must_find([1, 2, 3], fn(input) { input == 4 }) == 4
}

/// Check that a ByteArray list is sorted
/// and return the total number of items seen.
/// If any two items are equal this function will fail.
/// This means that the list must also be unique.
/// Each item in the list must be 28 bytes or this
/// function will fail.
pub fn sort_and_count(self: List<ByteArray>) -> Int {
  when self is {
    [] -> 0
    [x] -> {
      expect builtin.length_of_bytearray(x) == 28
      1
    }
    [x, ..rest] -> {
      expect [y, ..] = rest
      expect builtin.length_of_bytearray(x) == 28
      expect builtin.less_than_bytearray(x, y)
      1 + sort_and_count(rest)
    }
  }
}

test sort_and_count_1() {
  sort_and_count([]) == 0
}

test sort_and_count_2() {
  sort_and_count(
    [
      #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    ],
  ) == 3
}

test sort_and_count_3() fail {
  sort_and_count(
    [
      #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
      #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    ],
  ) == 3
}

test sort_and_count_4() {
  sort_and_count([#"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]) == 1
}

test sort_and_count_5() fail {
  sort_and_count([#"aa"]) == 1
}
