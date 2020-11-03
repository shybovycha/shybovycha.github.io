---
layout: post
title: 'Linq is awesome, right?'
date: '2019-12-11T12:49:39+10:00'
---

Recently I've seen a curious blog on Dev.to, [C# and .NET Core Appreciation Post. The most beautiful piece of code I have ever seen... this month!](https://dev.to/sduduzog/c-and-net-core-appreciation-post-the-most-beautiful-piece-of-code-i-have-ever-seen-this-month-49gf).

In short, author was amazed by how beautiful Linq is, supporting their feelings with this code snippet:

```csharp
public User GetUserByToken(string token) {
  var user = (from u in Users
              join t in Tokens on u.Id equals t.UserId
              where t.Body == token
              select u).SingleOrDefault();

  return user;
}
```

Let's tear this example apart and check how really <del>performant</del> beautiful Linq is!

The boilerplate code to make the thing not fall apart:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

class User {
    public string Id { get; set; }

    public User(string Id) {
        this.Id = Id;
    }
}

class Token {
    public string UserId { get; set; }

    public string Body { get; set; }

    public Token(string UserId, string Body) {
        this.UserId = UserId;
        this.Body = Body;
    }
}

class Context {
    public List<User> Users = new List<User>();
    public List<Token> Tokens = new List<Token>();

    public User GetUserByToken(string token) {
        var user = (from u in Users
                    join t in Tokens on u.Id equals t.UserId
                    where t.Body == token
                    select u).SingleOrDefault();

        return user;
    }
}

public class MainClass {
    public static void Main(string[] args) {
        Context context = new Context();

        context.Users.Add(new User("1001"));
        context.Users.Add(new User("1002"));
        context.Users.Add(new User("1006"));

        context.Tokens.Add(new Token("1002", "Weirdo"));
        context.Tokens.Add(new Token("1001", "Waldo"));

        // at this point we want to start counting accesses from scratch
        Console.WriteLine("---");

        Console.WriteLine("User by token {0} = {1}", "Waldo", context.GetUserByToken("Waldo"));
    }
}
```

I am going to modify it slightly for the sake of metrics a bit later, but first let us see what
code does C# generate in order for those magical SQL-like operators to do their thing. I am using https://sharplab.io/ for that purpose.

The code generated for `Context#GetUserByToken`:

```csharp
public User GetUserByToken(string token)
{
    <>c__DisplayClass2_0 <>c__DisplayClass2_ = new <>c__DisplayClass2_0();
    <>c__DisplayClass2_.token = token;
    return Enumerable.SingleOrDefault(Enumerable.Select(Enumerable.Where(Enumerable.Join(Users, Tokens, <>c.<>9__2_0 ?? (<>c.<>9__2_0 = new Func<User, string>(<>c.<>9.<GetUserByToken>b__2_0)), <>c.<>9__2_1 ?? (<>c.<>9__2_1 = new Func<Token, string>(<>c.<>9.<GetUserByToken>b__2_1)), <>c.<>9__2_2 ?? (<>c.<>9__2_2 = new Func<User, Token, <>f__AnonymousType0<User, Token>>(<>c.<>9.<GetUserByToken>b__2_2))), new Func<<>f__AnonymousType0<User, Token>, bool>(<>c__DisplayClass2_.<GetUserByToken>b__3)), <>c.<>9__2_4 ?? (<>c.<>9__2_4 = new Func<<>f__AnonymousType0<User, Token>, User>(<>c.<>9.<GetUserByToken>b__2_4))));
}
```

Now I am adding a simple debug output to check the calls LINQ makes whenever we call `Context#GetUserByToken`.
To do that, I am simply going to smash `Console.WriteLine` statement to each getter of both `User` and `Tag` classes:

```csharp
class User {
    private string _Id;

    public string Id {
        get { Console.WriteLine("User<{0}>.Id was accessed", _Id); return _Id; }

        set { _Id = value; }
    }

    public User(string Id) {
        this.Id = Id;
    }
}

class Token {
    private string _UserId;
    private string _Body;

    public string UserId {
        get { Console.WriteLine("Token<{0}>.UserId was accessed", _Body); return _UserId; }

        set { _UserId = value; }
    }

    public string Body {
        get { Console.WriteLine("Token<{0}>.Body was accessed", _Body); return _Body; }

        set { _Body = value; }
    }

    public Token(string UserId, string Body) {
        this.UserId = UserId;
        this.Body = Body;
    }
}
```

Seems like LINQ goes through all users and all their tokens:

```
---
Token<Weirdo>.UserId was accessed
Token<Waldo>.UserId was accessed
User<1001>.Id was accessed
Token<Waldo>.Body was accessed
User<1002>.Id was accessed
Token<Weirdo>.Body was accessed
User<1006>.Id was accessed
```

If I was to add one more user like this

```csharp
context.Users.Add(new User("1008"));
```

then the lookup iterates over those ones too:

```
---
Token<Weirdo>.UserId was accessed
Token<Waldo>.UserId was accessed
User<1001>.Id was accessed
Token<Waldo>.Body was accessed
User<1002>.Id was accessed
Token<Weirdo>.Body was accessed
User<1006>.Id was accessed
User<1008>.Id was accessed
```

If I assign a token to that user, it also will be visited:

```csharp
context.Tokens.Add(new Token("1008", "Quattro"));
```

```
---
Token<Weirdo>.UserId was accessed
Token<Waldo>.UserId was accessed
Token<Quattro>.UserId was accessed
User<1001>.Id was accessed
Token<Waldo>.Body was accessed
User<1002>.Id was accessed
Token<Weirdo>.Body was accessed
User<1006>.Id was accessed
User<1008>.Id was accessed
Token<Quattro>.Body was accessed
```

This is _similar_ to the very naive implementation with two nested loops:

```csharp
public User GetUserByToken(string token) {
    foreach (User u in Users) {
        foreach (Token t in Tokens) {
            if (t.UserId == u.Id && t.Body == token) {
                return u;
            }
        }
    }

    return null;
}
```

The difference is in the exact environment it is running in:

```
---
Token<Weirdo>.UserId was accessed
User<1001>.Id was accessed
Token<Waldo>.UserId was accessed
User<1001>.Id was accessed
Token<Waldo>.Body was accessed
```

So in the good-case-scenario, naive implementation does fewer checks than the LINQ code.

The issue with both approaches is in the worst-case-scenario, which, as I have learned by heart, is the only possible scenario.
They both are going to run in `O(n^2)` time at worst.

Let's consider a small optimization, using _indexes_:

```csharp
class Context {
    private Dictionary<string, User> UserByToken = new Dictionary<string, User>();

    private Dictionary<string, User> UserById = new Dictionary<string, User>();

    public void AddUser(User u) {
      Users.Add(u);

      UserById[u.Id] = u;
    }

    public void AddToken(Token t) {
      Tokens.Add(t);

      UserByToken[t.Body] = UserById[t.UserId];
    }

    public User GetUserByToken2(string token) {
        return UserByToken[token];
    }
}

// then the main method needs to be adjusted correspondingly:
public class MainClass {
    public static void Main(string[] args) {
        Context context = new Context();

        context.AddUser(new User("1001"));
        context.AddUser(new User("1002"));
        context.AddUser(new User("1006"));
        context.AddUser(new User("1008"));

        context.AddToken(new Token("1002", "Weirdo"));
        context.AddToken(new Token("1001", "Waldo"));
        context.AddToken(new Token("1008", "Quattro"));

        // at this point we want to start counting accesses from scratch
        Console.WriteLine("---");

        Console.WriteLine("User by token {0} = {1}", "Waldo", context.GetUserByToken("Waldo"));
    }
}
```

Essentially, whenever we add any user or tag, we put them in a dictionary _(an index)_ in memory.

Okay, we sacrifice some memory, but do we earn anything from it?

The output of the program is very short:

```
User<1001>.Id was accessed
User<1002>.Id was accessed
User<1006>.Id was accessed
User<1008>.Id was accessed
Token<Weirdo>.Body was accessed
Token<Weirdo>.UserId was accessed
Token<Waldo>.Body was accessed
Token<Waldo>.UserId was accessed
Token<Quattro>.Body was accessed
Token<Quattro>.UserId was accessed
---
User by token Waldo = User
```

So essentially, no collection is iterated. The access to the required object is immediate.

One might argue that the examples here are very synthetic and do not showcase anything.
But imagine a real product with millions or even billions of entities like those.
Think an average Google-size organization using Jira. Now replace `User` with `Issue` and `Token` with `Comment`.
I can assure you, the numbers would be 10-folded.

Contra: the code is written to be read (more often than not) and deleted (my favourite scenario).
So even thought hashmaps might seem appealing, indexing on few more fields might make the code hard to maintain.
This is especially crucial for long-lasting projects, where developers come and go as the time passes.
So _sometimes_ this might be a necessary evil.

But sometimes it is best to know the theory rather than the tool.
