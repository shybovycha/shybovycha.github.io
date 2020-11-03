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

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

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

        Console.WriteLine("User by token {0} = {1}", "Waldo", context.GetUserByToken("Waldo"));
        // Console.WriteLine("User by token {0} = {1}", "Bob", context.GetUserByToken("Bob"));
        // Console.WriteLine("User by token {0} = {1}", "Weirdo", context.GetUserByToken("Weirdo"));
    }
}
```

generates this code for `Context#GetUserByToken`:

```csharp
public User GetUserByToken(string token)
{
    <>c__DisplayClass2_0 <>c__DisplayClass2_ = new <>c__DisplayClass2_0();
    <>c__DisplayClass2_.token = token;
    return Enumerable.SingleOrDefault(Enumerable.Select(Enumerable.Where(Enumerable.Join(Users, Tokens, <>c.<>9__2_0 ?? (<>c.<>9__2_0 = new Func<User, string>(<>c.<>9.<GetUserByToken>b__2_0)), <>c.<>9__2_1 ?? (<>c.<>9__2_1 = new Func<Token, string>(<>c.<>9.<GetUserByToken>b__2_1)), <>c.<>9__2_2 ?? (<>c.<>9__2_2 = new Func<User, Token, <>f__AnonymousType0<User, Token>>(<>c.<>9.<GetUserByToken>b__2_2))), new Func<<>f__AnonymousType0<User, Token>, bool>(<>c__DisplayClass2_.<GetUserByToken>b__3)), <>c.<>9__2_4 ?? (<>c.<>9__2_4 = new Func<<>f__AnonymousType0<User, Token>, User>(<>c.<>9.<GetUserByToken>b__2_4))));
}
```

and it does run through each of the combinations of users and tokens:

```
Token<Weirdo>.UserId was accessed
Token<Waldo>.UserId was accessed
User<1001>.Id was accessed
Token<Waldo>.Body was accessed
User<1002>.Id was accessed
Token<Weirdo>.Body was accessed
User<1006>.Id was accessed
```

adding one more user:

```
Token<Weirdo>.UserId was accessed
Token<Waldo>.UserId was accessed
User<1001>.Id was accessed
Token<Waldo>.Body was accessed
User<1002>.Id was accessed
Token<Weirdo>.Body was accessed
User<1006>.Id was accessed
User<1010>.Id was accessed
```

adding one more token:

```
Token<Weirdo>.UserId was accessed
Token<Waldo>.UserId was accessed
Token<Bob>.UserId was accessed
User<1001>.Id was accessed
Token<Waldo>.Body was accessed
Token<Bob>.Body was accessed
User<1002>.Id was accessed
Token<Weirdo>.Body was accessed
User<1006>.Id was accessed
```

let's consider this optimization:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

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

class Context {
    public List<User> Users = new List<User>();
    public List<Token> Tokens = new List<Token>();

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

    public User GetUserByToken1(string token) {
        var user = (from u in Users
                    join t in Tokens on u.Id equals t.UserId
                    where t.Body == token
                    select u).SingleOrDefault();

        return user;
    }

    public User GetUserByToken2(string token) {
        return UserByToken[token];
    }
}

public class MainClass {
    public static void Main(string[] args) {
        Context context = new Context();

        context.AddUser(new User("1001"));
        context.AddUser(new User("1002"));
        context.AddUser(new User("1006"));

        context.AddToken(new Token("1002", "Weirdo"));
        context.AddToken(new Token("1001", "Waldo"));
        context.AddToken(new Token("1001", "Bob"));
        context.AddToken(new Token("1001", "Sam"));

        Console.WriteLine("====");

        Console.WriteLine("User by token {0} = {1}", "Waldo", context.GetUserByToken1("Waldo"));

        Console.WriteLine("====");

        Console.WriteLine("User by token {0} = {1}", "Waldo", context.GetUserByToken2("Waldo"));
        // Console.WriteLine("User by token {0} = {1}", "Bob", context.GetUserByToken("Bob"));
        // Console.WriteLine("User by token {0} = {1}", "Weirdo", context.GetUserByToken("Weirdo"));
    }
}
```

quick stats:

```
User<1001>.Id was accessed
User<1002>.Id was accessed
User<1006>.Id was accessed
Token<Weirdo>.Body was accessed
Token<Weirdo>.UserId was accessed
Token<Waldo>.Body was accessed
Token<Waldo>.UserId was accessed
Token<Bob>.Body was accessed
Token<Bob>.UserId was accessed
Token<Sam>.Body was accessed
Token<Sam>.UserId was accessed
====
Token<Weirdo>.UserId was accessed
Token<Waldo>.UserId was accessed
Token<Bob>.UserId was accessed
Token<Sam>.UserId was accessed
User<1001>.Id was accessed
Token<Waldo>.Body was accessed
Token<Bob>.Body was accessed
Token<Sam>.Body was accessed
User<1002>.Id was accessed
Token<Weirdo>.Body was accessed
User<1006>.Id was accessed
User by token Waldo = User
====
<EOF>
```
