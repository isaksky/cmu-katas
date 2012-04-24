# Had some problems with lazy recursive list in clojure, so did this one in ruby first
require 'set'
BASE_BOOK_PRICE = 8.00
PCT_DISCOUNT = Hash.new(0.0).merge  2 => 0.05, 3 => 0.10, 4 => 0.20, 5 => 0.25

def price bundle
  pct_discount = PCT_DISCOUNT[bundle.uniq.count]
  unit_price = BASE_BOOK_PRICE - (BASE_BOOK_PRICE * pct_discount)
  bundle.count * unit_price
end

# E.g., book_set([1,1,2,1,3]) => [1, 2, 3, 3, 4, 5, 5, 5]
def book_set book_frequencies
  books = []
  book_frequencies.each_with_index do |freq, i|
    book_number = i + 1
    freq.times { books << book_number }
  end
  books
end

def all_book_combinations unique_potter_books
  combos = []
  (2..5).each do |n|
    unique_potter_books.to_a.combination(n).each do |combo|
      combos << combo
    end
  end
  combos
end

# E.g., frequencies([1,5,5]) => {1 => 1, 5 => 2}
def frequencies ary
  ary.reduce Hash.new(0) do |h, v|
    h.merge v => h[v] + 1
  end
end

# E.g., list_subtract([1,1,2], [1,3,4]) => [1,2]
def list_subtract a1, a2
  a1_freqs, a2_freqs = [frequencies(a1), frequencies(a2)]
  a1_freqs.reduce([]) do |memo, (v, freq)|
    (freq - a2_freqs[v]).times { memo << v }
    memo
  end
end

def all_book_combo_combos books, work = [], &block
  book_combinations = all_book_combinations books.to_set
  yield work.clone.push books if book_combinations.empty?

  book_combinations.each do |combo|
    books_left = list_subtract books, combo
    all_book_combo_combos(books_left, work.clone.push(combo), &block)
  end
end

def find_cheapest_combo books
  cheapest_bundle = cheapest_bundle_price = nil
  num_tested = 0
  all_book_combo_combos books do |combo|
    price = combo.map{|c| price(c)}.reduce(:+)
    puts "Price: #{price.round(4)}. Bundle: #{combo}"
    if cheapest_bundle.nil? || price < cheapest_bundle_price
      cheapest_bundle_price = price
      cheapest_bundle = combo
    end
    num_tested += 1
  end
  puts "Tested #{num_tested} combos."
  [cheapest_bundle_price, cheapest_bundle]
end

if __FILE__ == $0
  sample_books = [2,2,2,1,1]
  puts "Trying #{sample_books}"
  price, bundle = find_cheapest_combo book_set sample_books
  puts "Price #{price} \t Bundles #{bundle}"
end
