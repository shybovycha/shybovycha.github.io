---
title: "Pains of a C++ developer"
tags: [cpp, c++, programming]
---

I have tried to modernize my old chess project.
It is a fairly simple project, written in C++ and PHP (server, for multiplayer). It renders the GUI using SFML and loads configs from XML file, so it has two external dependencies.
The idea I had was to extract a FEN parser and a move (in algebraic notation) validator into a separate library and consume it in both server and client. This would significantly reduce the code duplication.
I made the library and decided to cover it with sufficient unit tests with GoogleTest, adding a new dependency.
The last improvement idea on the list was to use gRPC and protobuf to unify the communication protocol between client and server.
Since protobuf files and libraries are kind of a pain to compile with CMake (which is, to my greatest regrets, the most popular tool in C++ world; yet one of the shittiest tools I have ever used, in my humble opinion),
I sought a better build system. I used XMake in the past and it proven to be very easy to setup and support even dependencies from vcpkg.
When rewriting the library, I did it on my Mac. But for setting up the XMake project I used my Windows machine with a brand new Visual Studio 2022 installed.
Oh how surprised I was to figure it completely misled me into thinking VC compiler is C++20 complaint!

```
C:\Users\shybo\source\repos\moo-chess\lib\src\ChessLib.hpp(77): error C2512: 'std::hash<ChessLib::Move>': no appropriate default constructor available
C:\Users\shybo\source\repos\moo-chess\lib\src\ChessLib.hpp(77): note: Invalid aggregate initialization
C:\Users\shybo\source\repos\moo-chess\lib\src\ChessLib.hpp(77): error C2512: 'std::hash<ChessLib::Move>': no appropriate default constructor available
C:\Users\shybo\source\repos\moo-chess\lib\src\ChessLib.hpp(77): note: Invalid aggregate initialization
C:\Users\shybo\source\repos\moo-chess\lib\src\ChessLib.hpp(255): error C2908: explicit specialization; 'std::hash<ChessLib::Move>' has already been instantiated
C:\Users\shybo\source\repos\moo-chess\lib\src\ChessLib.hpp(255): error C2766: explicit specialization; 'std::hash<ChessLib::Move>' has already been defined
C:\Users\shybo\source\repos\moo-chess\lib\src\ChessLib.hpp(255): note: see previous definition of 'std::hash<ChessLib::Move>'
src\ChessLib.cpp(270): error C2976: 'std::vector': too few template arguments
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(270): error C2641: cannot deduce template arguments for 'std::vector'
src\ChessLib.cpp(270): error C2783: 'std::vector<<unnamed-symbol>,<unnamed-symbol>> std::vector(void)': could not deduce template argument for '<unnamed-symbol>'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(270): error C2783: 'std::vector<<unnamed-symbol>,<unnamed-symbol>> std::vector(void)': could not deduce template argument for '<unnamed-symbol>'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(270): error C2780: 'std::vector<<unnamed-symbol>,<unnamed-symbol>> std::vector(std::vector<<unnamed-symbol>,<unnamed-symbol>>)': expects 1 arguments - 0 provided
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(282): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(293): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(299): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(302): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(303): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(312): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(312): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(312): error C2672: 'std::find_if': no matching overloaded function found
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\xutility(6410): note: could be '_InIt std::find_if(_InIt,const _InIt,_Pr)'
src\ChessLib.cpp(312): note: '_InIt std::find_if(_InIt,const _InIt,_Pr)': expects 3 arguments - 2 provided
src\ChessLib.cpp(314): error C3536: 'position': cannot be used before it is initialized
src\ChessLib.cpp(314): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(319): error C2100: you cannot dereference an operand of type 'int'
src\ChessLib.cpp(328): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(328): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(328): error C2672: 'std::find_if': no matching overloaded function found
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\xutility(6410): note: could be '_InIt std::find_if(_InIt,const _InIt,_Pr)'
src\ChessLib.cpp(328): note: '_InIt std::find_if(_InIt,const _InIt,_Pr)': expects 3 arguments - 2 provided
src\ChessLib.cpp(330): error C3536: 'position': cannot be used before it is initialized
src\ChessLib.cpp(330): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(335): error C2100: you cannot dereference an operand of type 'int'
src\ChessLib.cpp(339): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(339): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(339): error C2672: 'std::find_if': no matching overloaded function found
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\xutility(6410): note: could be '_InIt std::find_if(_InIt,const _InIt,_Pr)'
src\ChessLib.cpp(339): note: '_InIt std::find_if(_InIt,const _InIt,_Pr)': expects 3 arguments - 2 provided
src\ChessLib.cpp(341): error C3536: 'position': cannot be used before it is initialized
src\ChessLib.cpp(341): error C2027: use of undefined type 'std::vector'
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\include\format(85): note: see declaration of 'std::vector'
src\ChessLib.cpp(346): error C2100: you cannot dereference an operand of type 'int'
```

And all of the above are generated in both CMake and XMake setups, so it is not a misconfiguration issue.

The complains are about this piece of code:

```cpp
std::vector<Position> fromCandidates;
```

where `Position` is defined as following:

```cpp
struct Position {
    unsigned int row;
    char col;

    void parse(const std::string& positionString) {
        col = positionString.at(0);
        row = static_cast<int>(positionString.at(1)) - static_cast<int>('1') + 1;
    }

    bool operator==(const Position& other) const = default;

    friend std::ostream& operator<<(std::ostream& os, const Position& pos) {
        return os << std::format("<{1}, {0}>", static_cast<char>(pos.col), pos.row);
    }
};

template<>
struct std::hash<Position> {
    std::size_t operator()(Position const& pos) const noexcept {
        std::size_t h1 = std::hash<int>{}(pos.row);
        std::size_t h2 = std::hash<char>{}(pos.col);
        return h1 ^ (h2 << 1);
    }
};
```

But apparently, this was caused by Qt Creator suggesting to remove the `#include <vector>` in the header file, since the `std::vector` is used in the source file, but not in the header file itself.

Cool. But then, the tests started to fail on Windows - partially caused by `std::option` being `std::nullopt` on Windows and for some other unknown reason:

```
[  FAILED  ] ParsingMoveTest.ParsingCastling3
[  FAILED  ] ParsingMoveTest.ParsingCastling4
[  FAILED  ] ValidatingKingMoveTest.Castling7
```