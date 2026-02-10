---
layout: post
title: 'Nested loops vs Streams flatMap chain'
tags: ['java', 'optimizations', 'performance']
---

Recently we had a discussion in my team about the performance of the following code:

```java
for (Database database : getDatabases().values()) {
    for (Schema schema : database.getSchemas().values()) {
        for (Table table : schema.getTables().values()) {
            for (Column column : table.getColumns().values()) {
                processColumn(column);
            }
        }
    }
}
```

I thought that all these nested loops would need to backtrack on each new iteration and this would cause a lot of staggering (due to the need to jump all over the memory).
Additionally, the time complexity of this piece of code is `O(n^4)`.

My suggestion was to use `Stream#flatMap`:

```java
getDatabases().values().stream()
  .flatMap(database -> database.getSchemas().values().stream())
  .flatMap(schema -> schema.getTables().values().stream())
  .flatMap(table -> table.getColumns().values().stream())
  .forEach(column -> processColumn(column));
```

This code has linear complexity of `O(n)` and my assumption is that it would not create staggers because of backtracking (by creating intermediate views of a collection and only iterating over it).

In plain C i reckon this would looks something like this:

```cpp
for (const auto& [_, database] : getDatabases()) {
  for (const auto& [_, schema] : database->getSchemas()) {
    for (const auto& [_, table] : schema->getTables()) {
      for (const auto& [_, column] : table->getColumns()) {
        processColumn(column);
      }
    }
  }
}
```

compared to (deliberately not using ranges & views to highlight the idea):

```cpp
std::vector<Database> databases;
std::transform(getDatabases().begin(), getDatabases().end(), std::back_inserter(databases),
  [](const auto& pair) { return pair.second; });

std::vector<Schema> schemas;
for (const auto& database : databases) {
  std::transform(database.schemas.begin(), database.schemas.end(), std::back_inserter(schemas),
    [](const auto& pair) { return pair.second; });
}

std::vector<Table> tables;
for (const auto& schema : schemas) {
  std::transform(schema.tables.begin(), schema.tables.end(), std::back_inserter(tables),
    [](const auto& pair) { return pair.second; });
}

std::vector<Column> columns;
for (const auto& table : tables) {
  std::transform(table.columns.begin(), table.columns.end(), std::back_inserter(columns),
    [](const auto& pair) { return pair.second; });
}

for (const auto& column : columns) {
  processColumn(column);
}
```

or using C++20 ranges & views:

```cpp
auto databasesMap = getDatabases();

auto columns = databasesMap
  | std::views::values
  | std::views::transform([](const Database& db) { return db.schemas | std::views::values; })
  | std::views::join
  | std::views::transform([](const Schema& schema) { return schema.tables | std::views::values; })
  | std::views::join
  | std::views::transform([](const Table& table) { return table.columns | std::views::values; })
  | std::views::join;

for (const auto& column : columns) {
  processColumn(column);
}
```

So I went ahead with two-stage analysis of these two versions: first, I created a simplified (but somewhat complete) implementation in C++ and checked what output compiler generates at GoldBolt:

```cpp
#include <ranges>
#include <iostream>
#include <vector>
#include <map>
#include <string>

struct Column { std::string name; std::string type; };
struct Table { std::map<std::string, Column> columns; };
struct Schema { std::map<std::string, Table> tables; };
struct Database { std::map<std::string, Schema> schemas; };

std::map<std::string, Database> getDatabases() {
    return std::map<std::string, Database> {
        {
            "moo",
            { .schemas = std::map<std::string, Schema> {
                {
                    "public",
                    { .tables = std::map<std::string, Table> {
                        {
                            "users",
                            { .columns = std::map<std::string, Column> {
                                {
                                    "id",
                                    {
                                        .name = "id",
                                        .type = "INT"
                                    }
                                }
                            } }
                        }
                    } }
                }
            } }
        }
    };
}

void processColumn(Column col) {
    std::cout << col.name << "::" << col.type << std::endl;
}

int main() {
    for (const auto& [_, database] : getDatabases()) {
        for (const auto& [_, schema] : database.schemas) {
            for (const auto& [_, table] : schema.tables) {
                for (const auto& [_, column] : table.columns) {
                    processColumn(column);
                }
            }
        }
    }
    return 0;
}
```

and the output was... lond, very long. But it showed a few interesting bits (specifically, the x86_64 GCC v15.2 compiler):

* compiler generates `call    "_Unwind_Resume"` instructions
* the `std::map` implementation relies on red-black trees (presumingly): `call    "std::_Rb_tree_node_base* std::_Rb_tree<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >, std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table>, std::_Select1st<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table> >, std::less<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > >, std::allocator<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table> > >::_M_copy<false, std::_Rb_tree<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >, std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table>, std::_Select1st<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table> >, std::less<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > >, std::allocator<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table> > >::_Alloc_node>(std::_Rb_tree_node<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table> >*, std::_Rb_tree_node_base*, std::_Rb_tree<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >, std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table>, std::_Select1st<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table> >, std::less<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > >, std::allocator<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table> > >::_Alloc_node&)"`
* there are quite a few allocations, deallocations and memory copy operations: `call    "memcpy"`, `call    "operator new(unsigned long)"`, `call    "std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Schema>::~pair()"`, `call    "std::_Rb_tree<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >, std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table>, std::_Select1st<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table> >, std::less<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > >, std::allocator<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table> > >::_M_erase(std::_Rb_tree_node<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, Table> >*)"`, `call    "operator delete(void*, unsigned long)"`

the `std::transform` version though:

* it is twice as long output
* it is still filled with deallocations, but this time it is mostly about strings and longs: `call    "std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >::_M_dispose()"`, `"operator new(unsigned long)"`, `call    "operator delete(void*, unsigned long)"`, `call    "memcpy"`

the `std::ranges` version:

* the output is about as short as the nested loop version
* there are fewer `memcpy` calls, fewer `std::pair` allocations and deallocations - just tree nodes instead `call    std::_Rb_tree_node_base* std::_Rb_tree<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>>, std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>> const, Table>, std::_Select1st<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>> const, Table>>, std::less<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>>>, std::allocator<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>> const, Table>>>::_M_copy<false, std::_Rb_tree<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>>, std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>> const, Table>, std::_Select1st<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>> const, Table>>, std::less<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>>>, std::allocator<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>> const, Table>>>::_Alloc_node>(std::_Rb_tree_node<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>> const, Table>>*, std::_Rb_tree_node_base*, std::_Rb_tree<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>>, std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>> const, Table>, std::_Select1st<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>> const, Table>>, std::less<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>>>, std::allocator<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char>> const, Table>>>::_Alloc_node&)
`
* no destructor calls

To actually measure the performance, I created a benchmark:

```cpp
#include <ranges>
#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <chrono>
#include <algorithm>

struct Column { std::string name; std::string type; };
struct Table { std::map<std::string, Column> columns; };
struct Schema { std::map<std::string, Table> tables; };
struct Database { std::map<std::string, Schema> schemas; };

// Generate test data with configurable size
std::map<std::string, Database> generateDatabases(int numDatabases, int numSchemas, int numTables, int numColumns) {
    std::map<std::string, Database> databases;
    for (int d = 0; d < numDatabases; ++d) {
        Database db;
        for (int s = 0; s < numSchemas; ++s) {
            Schema schema;
            for (int t = 0; t < numTables; ++t) {
                Table table;
                for (int c = 0; c < numColumns; ++c) {
                    table.columns["col_" + std::to_string(c)] = {
                        .name = "col_" + std::to_string(c),
                        .type = "VARCHAR"
                    };
                }
                schema.tables["table_" + std::to_string(t)] = std::move(table);
            }
            db.schemas["schema_" + std::to_string(s)] = std::move(schema);
        }
        databases["db_" + std::to_string(d)] = std::move(db);
    }
    return databases;
}

volatile int sink = 0; // Prevent compiler from optimizing away the loop

void benchmarkNestedLoops(const std::map<std::string, Database>& databases) {
    int count = 0;
    for (const auto& [_, database] : databases) {
        for (const auto& [_, schema] : database.schemas) {
            for (const auto& [_, table] : schema.tables) {
                for (const auto& [_, column] : table.columns) {
                    ++count;
                }
            }
        }
    }
    sink = count;
}

void benchmarkSeparateVectors(const std::map<std::string, Database>& databasesMap) {
    std::vector<Database> databases;
    std::transform(databasesMap.begin(), databasesMap.end(), std::back_inserter(databases),
        [](const auto& pair) { return pair.second; });

    std::vector<Schema> schemas;
    for (const auto& database : databases) {
        std::transform(database.schemas.begin(), database.schemas.end(), std::back_inserter(schemas),
            [](const auto& pair) { return pair.second; });
    }

    std::vector<Table> tables;
    for (const auto& schema : schemas) {
        std::transform(schema.tables.begin(), schema.tables.end(), std::back_inserter(tables),
            [](const auto& pair) { return pair.second; });
    }

    std::vector<Column> columns;
    for (const auto& table : tables) {
        std::transform(table.columns.begin(), table.columns.end(), std::back_inserter(columns),
            [](const auto& pair) { return pair.second; });
    }

    int count = 0;
    for (const auto& column : columns) {
        ++count;
    }
    sink = count;
}

void benchmarkRanges(const std::map<std::string, Database>& databasesMap) {
    auto columns = databasesMap
        | std::views::values
        | std::views::transform([](const Database& db) { return db.schemas | std::views::values; })
        | std::views::join
        | std::views::transform([](const Schema& schema) { return schema.tables | std::views::values; })
        | std::views::join
        | std::views::transform([](const Table& table) { return table.columns | std::views::values; })
        | std::views::join;

    int count = 0;
    for (const auto& column : columns) {
        ++count;
    }
    sink = count;
}

template<typename Func>
double measureTime(Func&& func, int iterations) {
    auto start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < iterations; ++i) {
        func();
    }
    auto end = std::chrono::high_resolution_clock::now();
    return std::chrono::duration<double, std::milli>(end - start).count() / iterations;
}

int main() {
    // Generate test data: 10 databases, 10 schemas each, 10 tables each, 10 columns each
    // Total: 10 * 10 * 10 * 10 = 10,000 columns
    auto databases = generateDatabases(10, 10, 10, 10);
    
    const int iterations = 100;
    
    std::cout << "Benchmarking with " << iterations << " iterations...\n\n";
    
    double nestedTime = measureTime([&]() { benchmarkNestedLoops(databases); }, iterations);
    std::cout << "Nested loops:      " << nestedTime << " ms\n";
    
    double vectorsTime = measureTime([&]() { benchmarkSeparateVectors(databases); }, iterations);
    std::cout << "Separate vectors:  " << vectorsTime << " ms\n";
    
    double rangesTime = measureTime([&]() { benchmarkRanges(databases); }, iterations);
    std::cout << "Ranges & views:    " << rangesTime << " ms\n";
    
    std::cout << "\nRelative performance (lower is better):\n";
    std::cout << "Nested loops:      1.00x (baseline)\n";
    std::cout << "Separate vectors:  " << (vectorsTime / nestedTime) << "x\n";
    std::cout << "Ranges & views:    " << (rangesTime / nestedTime) << "x\n";
    
    return 0;
}
```

and the results:

with `-O1`:

```
Benchmarking with 100 iterations...

Nested loops:      0.0197833 ms
Separate vectors:  1.49174 ms
Ranges & views:    0.025335 ms

Relative performance (lower is better):
Nested loops:      1.00x (baseline)
Separate vectors:  75.404x
Ranges & views:    1.28062x
```

with `-O2`:

```
Benchmarking with 100 iterations...

Nested loops:      0.0280458 ms
Separate vectors:  1.59105 ms
Ranges & views:    0.0206242 ms

Relative performance (lower is better):
Nested loops:      1.00x (baseline)
Separate vectors:  56.7304x
Ranges & views:    0.735374x
```

with `-O3`:

```
Benchmarking with 100 iterations...

Nested loops:      0.0371354 ms
Separate vectors:  1.50797 ms
Ranges & views:    0.0332571 ms

Relative performance (lower is better):
Nested loops:      1.00x (baseline)
Separate vectors:  40.6073x
Ranges & views:    0.895563x
```