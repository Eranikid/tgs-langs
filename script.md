# Захотелось мне значит сделать формочку логина для веб приложения

Ну я взял свой любимый C# и сделал:
```c#
[ApiController]
[Route("/api/v1/[controller]")]
public class AccountController : ControllerBase {
    private readonly IDbConnection _db;

    public AccountController(IDbConnection db) {
        _db = db;
    }

    [HttpPost]
    public async Task<IActionResult> Create(CreateAccountRequest request) {
        var passwordHash = Argon2.Hash(request.Password);

        await _db.ExecuteAsync("INSERT INTO users (id, name, passwordHash) VALUES (@id, @name, @passwordHash)",
            new
            {
                id = Guid.NewGuid(),
                name = request.Name,
                passwordHash
            });

        return Ok();
    }
}
```

А потом подумал:
* А что если имя пользователя слишком короткое или слишком длинное? Надо вернуть 400 с понятной ошибкой
* А что если пароль слишком короткий или слишком длинный? Надо вернуть 400 с понятной ошибкой
* А если и то и другое невалидно, то надо бы вернуть обе ошибки, а не только первую случившуюся
* А что если имя пользователя уже занято и вставка в базу упала с нарушением UNIQUE CONSTRAINT? Надо вернуть 400 с понятной ошибкой
* А что если до базы не смогли достучаться? Надо вернуть 500 с понятной ошибкой
* Я что, зря всё это DDD читал? Чего у меня одни примитивы?

И переделал:
```c#
[ApiController]
[Route("/api/v1/[controller]")]
public class AccountController : ControllerBase {
    private readonly IDbConnection _db;

    public AccountController(IDbConnection db) {
        _db = db;
    }

    [HttpPost]
    public async Task<IActionResult> Create(CreateAccountRequest request) {
        Username username = null;
        Password password = null;
        var errors = new List<string>();

        try {
            username = Username.Of(request.Name);
        }
        catch (UsernameTooShortException) {
            errors.Add("USERNAME_TOO_SHORT");
        }
        catch (UsernameTooLongException) {
            errors.Add("USERNAME_TOO_LONG");
        }

        try {
            password = Password.Of(request.Password);
        }
        catch (PasswordTooShortException) {
            errors.Add("PASSWORD_TOO_SHORT");
        }
        catch (PasswordTooLongException) {
            errors.Add("PASSWORD_TOO_LONG");
        }

        if (errors.Any()) {
            return BadRequest(new
            {
                errors
            });
        }

        var passwordHash = PasswordHash.Of(password);

        try {
            await _db.ExecuteAsync("INSERT INTO users (id, name, passwordHash) VALUES (@id, @name, @passwordHash)",
                new
                {
                    id = Guid.NewGuid(),
                    name = username,
                    passwordHash
                });
        }
        catch (DuplicateKeyEntryException) {
            return BadRequest(new
            {
                errors = new[]
                {
                    "USERNAME_ALREADY_TAKEN"
                }
            });
        }
        catch {
            return StatusCode(500, new
            {
                errors = new[]
                {
                    "UNEXPECTED_DATABASE_ERROR"
                }
            });
        }

        return Ok();
    }
}
```

В чём проблема:
* Много разных вываливающихся из метода эксепшенов сложно и многословно обрабатывать и неудобно комбинировать
* Можно задокументировать все вываливающиеся из метода exception'ы через `<exception>` комменты xmldoc'а, но это не даст нам никаких compile-time проверок (по крайней мере без дополнительных приседаний).

На самом деле обе эти проблемы решаются одним паттерном - Discriminated union'ами.

Сейчас мы перепишем этот C# код с использованием discriminated union'ов...

...не перепишем, потому что [пропозал](https://github.com/dotnet/csharplang/issues/113) пока всё ещё открыт.

Есть [библиотеки](https://github.com/mcintyre321/OneOf) на гитхабе, которые пытаются закрыть эту дыру имеющимися средствами, но получается, как несложно догадаться, всё равно неуклюже.

## Промежуточные итоги
|   | Наличие DU | 
|---|---|
| C# | ❌ |

## Так и что делать-то теперь?

Ну, discriminated union'ы, это же функциональная фича, а какой у нас функциональный язык? Правильно, Haskell! Запасаемся мануалами и попробуем:
```haskell
-- Создаём "класс" для запроса
data CreateAccountRequest = CreateAccountRequest { username :: String
                                                 , password :: String
                                                 } deriving (Show)

-- <ТёмнаяФункцинальнаяМагия>
newtype Validation e r = Validation (Either e r) deriving (Eq, Show, Functor)

instance Monoid m => Applicative (Validation m) where
  pure = Validation . pure
  Validation (Left x) <*> Validation (Left y) = Validation (Left (mappend x y))
  Validation f         <*> Validation r         = Validation (f <*> r)

instance Monoid m => Monad (Validation m) where
    Validation (Left  l) >>= _ = Validation (Left l)
    Validation (Right r) >>= k = k r
-- </ТёмнаяФункцинальнаяМагия>

-- "класс" имя пользователя
data Username = Username String deriving (Show)
-- "класс" ошибка в имени пользователя
-- Имена `TooShort` и `TooLong` конфликтуют с такими же именами
-- в `PasswordError`, а я не умею в хаскель и не знаю как их зарезолвить,
-- поэтому просто префикс
data UsernameError = UNTooShort | UNTooLong deriving (Show)

-- Функция `createUsername` принимает строку и возвращает либо ошибку, либо имя
createUsername :: String -> Either UsernameError Username
createUsername val
  | length val < 3  = Left UNTooShort
  | length val > 25 = Left UNTooLong
  | otherwise       = Right $ Username val

-- Всё то же самое для пароля
data Password = Password String deriving (Show)
data PasswordError = PTooShort | PTooLong deriving (Show)

createPassword :: String -> Either PasswordError Password
createPassword val
  | length val < 6   = Left PTooShort
  | length val > 250 = Left PTooLong
  | otherwise        = Right $ Password val

-- "класс" ошибка взаимодействия с БД
data DatabaseError = UsernameTaken | DbUnavailable deriving (Show)

-- функция `saveToDb` принимает имя и пароль, и возвращает либо ошибку, либо `IO ()`
-- `IO ()` это примерно то же самое что `Task<void>`
saveToDb :: Username -> Password -> Either DatabaseError (IO ())
saveToDb uname pwd = Right $ print (uname, pwd)
-- Можно раскомментить чтобы проверить что-то, кроме happy path
-- saveToDb uname pwd = Left $ UsernameTaken
-- saveToDb uname pwd = Left $ DbUnavailable

-- Для нашего главного метода нам нужен будет общий тип ошибки
-- Создаём тип, который заворачивает ошибку либо имени, либо пароля, либо БД
data HandlerError =
  PError PasswordError
  | UNError UsernameError
  | DBError DatabaseError
  deriving (Show)

-- Я не умею в хаскель и не знаю как вот это сделать нормально
-- Поэтому у меня 3 одинаковых функции, которые приводят разные ошибки к типу `HandlerError`
-- (и заворачивают в массив)
castUNError :: Either UsernameError a -> Validation [HandlerError] a
castUNError (Right v) = Validation $ Right v
castUNError (Left e) = Validation $ Left $ [UNError e]

castPError :: Either PasswordError a -> Validation [HandlerError] a
castPError (Right v) = Validation $ Right v
castPError (Left e) = Validation $ Left $ [PError e]

castDBError :: Either DatabaseError a -> Validation [HandlerError] a
castDBError (Right v) = Validation $ Right v
castDBError (Left e) = Validation $ Left $ [DBError e]

-- Самое мясо
-- Очень элегантно (не считая castXError) описываем поток данных, а аппликативная валидация
-- за нас решает в какой момент всё пошло не так, и какие ошибки вернуть
handler :: CreateAccountRequest -> Validation [HandlerError] (IO ())
handler request = do
  uname <- castUNError $ createUsername $ username request
  pwd <- castPError $ createPassword $ password request
  castDBError $ saveToDb uname pwd

main = case handlerResult of
  (Validation (Right io)) -> io
  (Validation (Left err)) -> print err
  where
    request = CreateAccountRequest { username = "John_Doe", password = "P@sw0rd!" } 
    handlerResult = handler request 
```

Что можно сказать? Основная логика получилась намного лучше и элегантнее, чем в C#, но, с моей точки зрения, у Haskell две беды:

1. Когнитивная сложность. Катаморфизмы, бифункторы и прочие комонады, это не то, о чём лично мне хочется думать, когда я пишу код. Может это и круто, удобно, и позволяет подобно божеству выражать в коде концепции любой сложности выразительно и немногословно, но это очень сильно задирает порог входа.
2. Поддержка IDE. Я пробовал несколько вариаций, и Typescript в чистой Visual Studio Code удобнее любой из них

Что у C# по этим параметрам? Когнитивная сложность C# - это как раз то, что мне подходит, и он получает ⭐, а по поддержке IDE - в Omnisharp плохенько, в Visual Studio некроссплатформенно, а в ReSharper за деньги, поэтому просто ✅

## Промежуточные итоги

|   | Наличие DU | Когнитивная сложность | Поддержка IDE
|---|---|---|---|
| C# | ❌ | ⭐ | ✅ |
| Haskell | ✅ | ❌ | ❌ |

