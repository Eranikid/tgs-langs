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
  -- пропустим хэширование пароля
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

Что у C# по этим параметрам? Когнитивная сложность C# - это как раз то, что мне подходит, а по поддержке IDE - в Omnisharp плохенько, в Visual Studio некроссплатформенно, а в ReSharper за деньги, поэтому просто ✅

## Промежуточные итоги

|   | Наличие DU | Когнитивная сложность | Поддержка IDE
|---|---|---|---|
| C# | ❌ | ✅ | ✅ |
| Haskell | ✅ | ❌ | ❌ |

## А где же взять ЯП, с discriminated union'ами, хорошей поддержкой IDE, на котором можно писать без PhD по абстрактной алгебре?

Все слышали, а кто-то даже писал на Kotlin. Вы только посмотрите на страницу [код сниппетов](https://kotlinlang.org/docs/reference/idioms.html), там всё так интуитивно понятно, сразу ощущаешь что пишешь на языке созданном людьми для людей.

И бесплатная версия IntelliJ Idea от JetBrains буквально создана для Kotlin, так что с поддержкой IDE проблем не возникает. Давайте попробуем реализовать нашу задачку:
```kotlin
package mypackage

// Импортируем, чтобы можно было использовать `Ok` и `Err` без префикса `Result.`
import mypackage.Result.*

// Это будет класс `Either` из нашего примера на Haskell
// Обратите внимание, что `sealed class` в Kotlin работает не так, как в C#
// Тут `sealed` означает, что все наследники класса известны в compile-time, что и даёт нам discriminated union
sealed class Result<T, E> {
    class Ok<T, E>(val v: T) : Result<T, E>();
    class Err<T, E>(val e: E) : Result<T, E>();

    // Эта функция берёт значение из Ok, и прогоняет его через переданный делегат, который в свою очередь возвращает тоже Result
    // Err эта функция возвращает как есть
    // Ok(3).flatMap { Ok(it + 2) } == Ok(5)
    // Err("WRONG").flatMap { Ok(it + 2) } == Err("WRONG")
    // Ok(3).flatMap { Err("FAIL") } == Err("FAIL")
    fun <T1> flatMap(f: (T) -> Result<T1, E>): Result<T1, E> = when (this) {
        is Ok -> f(this.v)
        is Err -> Err(this.e)
    }

    // Эта функция трансформирует значение в Err, а Ok оставляет как есть
    // Ok(3).mapErr { "TOTAL " + it } == Ok(3)
    // Err("FAILURE").mapErr { "TOTAL " + it } == Err("TOTAL FAILURE")
    fun <E1> mapErr(f: (E) -> E1): Result<T, E1> = when (this) {
        is Ok -> Ok(this.v)
        is Err -> Err(f(this.e))
    }

    // Эта функция комбинирует этот результат с другим
    // Если они оба Ok, то возвращает кортеж с двумя значениями,
    // если хотя бы один Err - возвращает список со всеми ошибками
    fun <T1> combineWith(other: Result<T1, E>): Result<Pair<T, T1>, List<E>> = when (this) {
        is Ok -> when (other) {
            is Ok -> Ok(Pair(this.v, other.v))
            is Err -> Err(listOf(other.e))
        }
        is Err -> when (other) {
            is Ok -> Err(listOf(this.e))
            is Err -> Err(listOf(this.e, other.e))
        }
    }
}

// Тип для ошибки в имени
enum class UsernameError {
    TooShort, TooLong
}

// Тип для самого имени
class Username private constructor(val value: String) {
    companion object {
        fun new(value: String): Result<Username, UsernameError> {
            if (value.length < 3) return Err(UsernameError.TooShort)
            if (value.length > 32) return Err(UsernameError.TooLong)

            return Ok(Username(value))
        }
    }
}

// Тип для ошибки в пароле
enum class PasswordError {
    TooShort, TooLong
}

// Тип для самого пароля
class Password private constructor(val value: String) {
    companion object {
        fun new(value: String): Result<Password, PasswordError> {
            if (value.length < 6) return Err(PasswordError.TooShort)
            if (value.length > 250) return Err(PasswordError.TooLong)

            return Ok(Password(value))
        }
    }
}

// Тип для ошибки во взаимодействии с БД
enum class DatabaseError {
    DuplicateUsername, UnexpectedError
}

// Функция-заглушка для сохранения в БД
fun saveToDb(name: Username, password: Password): Result<Unit, DatabaseError> {
    return Ok(Unit)
}

// Общий тип для всех ошибок, которые происходят в нашем юзкейсе
sealed class HandlerError {
    class UsernameError(e: mypackage.UsernameError) : HandlerError()
    class PasswordError(e: mypackage.PasswordError) : HandlerError()
    class DatabaseError(e: mypackage.DatabaseError) : HandlerError()
}

// Функции-конвертеры для перевода частного типа в общий
fun toHandlerError(e: UsernameError): HandlerError = HandlerError.UsernameError(e)
fun toHandlerError(e: PasswordError): HandlerError = HandlerError.PasswordError(e)
fun toHandlerError(e: DatabaseError): HandlerError = HandlerError.DatabaseError(e)

// Самое мясо
// Обратите внимание что в Kotlin разрешён shadowing имён переменных -
// я могу переиспользовать имя переменной сколько угодно раз
fun createAccount(name: String, password: String): Result<Unit, List<HandlerError>> {
    val name = Username.new(name).mapErr(::toHandlerError)
    val password = Password.new(password).mapErr(::toHandlerError)

    return name.combineWith(password).flatMap {
        val (name, password) = it;
        saveToDb(name, password).mapErr(::toHandlerError).mapErr(::listOf)
    }
}

fun main() {
    val result = createAccount("John_Doe", "P@ssw0rd!")
    when (result) {
        is Ok -> println("User created!")
        is Err -> println("Errors: ${result.e}")
    }
}
```

Синтаксис оказался очень приятным. Я долго игрался с Kotlin, и уже было думал что наконец нашёл, что мне нужно, пока не попробовал добавить test coverage в проект.

Вещи, которые мы делаем MSBuild'ом и Cake'ом, в мире Kotlin делают при помощи Gradle. Gradle - это система сборки, работающая на JVM и использующая Kotlin в качестве билд скрипта (Можно ещё Groovy).

Вот так выглядит работа с тестами в Gradle-JUnit проекте:
1. Пишешь тест, запускаешь прогон тестов, все проходят, в консоли написано что всё хорошо
2. Меняешь код, несколько тестов падают, в консоли тебе пишет что какие-то тесты упали
3. Ты открываешь браузером сгенерённый html-тест репорт, где написано сколько тестов упали
4. Какие именно тесты упали ты так же смотришь через другую страничку тест репорта

Приведу ещё пример:

1. Я добавил как зависимость очень простой веб фреймворк в котором кроме роутинга ничего и нету
2. При запуске всё работало, но в консоли были ворнинги `SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder". SLF4J: Defaulting to no-operation (NOP) logger implementation`.
3. Как оказалось, в джаве DI работает через статический класс `ServiceLoader`, и мой фреймворк через этот статический класс пытается получить себе логгер (хотя его никто не просил!), а поскольку никаких зависимостей про логгинг я в свой проект не добавил, выплёвывается вот этот ворнинг.

В целом негативный опыт от котлина можно обобщить в категорию "Экосистема и тулчейн", и пользоваться этой экосистемой и тулчейном мне оказалось неприятно.

В C# и хаскелле экосистема тоже не фонтан, но с ними можно работать.

Зато котлин получает от меня приз зрительских симпатий за синтаксис.

## Промежуточные итоги

|   | Наличие DU | Когнитивная сложность | Поддержка IDE | Экосистема и тулчейн |
|---|---|---|---|---|
| C# | ❌ | ✅ | ✅ | ✅ |
| Haskell | ✅ | ❌ | ❌ | ✅ |
| Kotlin | ✅ | ✅ | ✅ | ❌ |

🏆 Синтаксис - **Kotlin**

## Ну давайте уже наконец напишем на Typescript!

А давайте:
```ts
// Нам также понадобится свой Either, реализуем его как абстрактный класс
abstract class Result<T, E> {
  // match принимает две функции, первая вызывается если результат - ok, другая - если err
  abstract match<R>(onOk: (v: T) => R, onErr: (e: E) => R): R;
  // Эта функция работает так же, как в котлин
  abstract flatMap<U, E2>(f: (v: T) => Result<U, E2>): Result<U, E | E2>;
  // И эта тоже
  abstract combineWith<U, E2>(other: Result<U, E2>): Result<[T, U], (E | E2)[]>;
}

// Хелперы, чтобы вместо new OkResult<...>(...) писать просто ok(...)
// Для конструкторов дженерик параметры не умеют инфериться, а для функций умеют
const ok = <T, E>(v: T): Result<T, E> => new OkResult<T, E>(v);
const err = <T, E>(e: E): Result<T, E> => new ErrResult<T, E>(e);

// Имплементация абстрактного класса раз
class OkResult<T, E> extends Result<T, E> {
  constructor(value: T) {
    super();
    this.value = value;
  }

  readonly value: T;

  match<R>(onOk: (v: T) => R, _: (e: E) => R): R {
    return onOk(this.value);
  }

  flatMap<U, E2>(f: (v: T) => Result<U, E2>): Result<U, E | E2> {
    return f(this.value);
  }

  combineWith<U, E2>(other: Result<U, E2>): Result<[T, U], (E | E2)[]> {
    return other.match(
      (v) => ok([this.value, v]),
      (e) => err([e])
    );
  }
}

// Имплементация абстрактного класса два
class ErrResult<T, E> extends Result<T, E> {
  constructor(error: E) {
    super();
    this.error = error;
  }

  readonly error: E;

  match<R>(_: (v: T) => R, onErr: (e: E) => R): R {
    return onErr(this.error);
  }

  flatMap<U, E2>(_: (v: T) => Result<U, E2>): Result<U, E | E2> {
    return err(this.error);
  }

  combineWith<U, E2>(other: Result<U, E2>): Result<[T, U], (E | E2)[]> {
    return other.match(
      (_) => err([this.error]),
      (e) => err([this.error, e])
    );
  }
}

// Тип ошибки имени
type UsernameError = "USERNAME_TOO_SHORT" | "USERNAME_TOO_LONG";

// Тип имени
class Username {
  readonly value: string;

  private constructor(value: string) {
    this.value = value;
  }

  static new(value: string): Result<Username, UsernameError> {
    if (value.length < 3) {
      return err("USERNAME_TOO_SHORT");
    } else if (value.length > 32) {
      return err("USERNAME_TOO_LONG");
    }

    return ok(new Username(value));
  }
}

// Тип ошибки пароля
type PasswordError = "PASSWORD_TOO_SHORT" | "PASSWORD_TOO_LONG";

// Тип пароля
class Password {
  readonly value: string;

  private constructor(value: string) {
    this.value = value;
  }

  static new(value: string): Result<Password, PasswordError> {
    if (value.length < 6) {
      return err("PASSWORD_TOO_SHORT");
    } else if (value.length > 250) {
      return err("PASSWORD_TOO_LONG");
    }

    return ok(new Password(value));
  }
}

// Тип ошибки БД
type DatabaseError = "USERNAME_TAKEN" | "UNEXPECTED_ERROR";

// Сохранение в БД
function saveToDb(
  username: Username,
  password: Password
): Result<void, DatabaseError> {
  return ok(undefined);
}

// Главная функция
function createAccount(aName: string, aPassword: string) {
  const name = Username.new(aName);
  const password = Password.new(aPassword);

  return name.combineWith(password).flatMap(([name, password]) => {
    return saveToDb(name, password);
  });
}
```

Отсюда видно, что discriminated union'ы работают в Typescript не так, как во всех остальных языках. С одной стороны, нам не пришлось объявлять отдельный тип ошибки для функции `createAccount`. Это потому, что Typescript умеет сам "соединять" и "разворачивать" такие типы.

К примеру если переменная `const name = Username.new(aName)` имеет тип `Result<Username, UsernameError>`, а переменная `const password = Password.new(aPassword)` имеет тип `Result<Password, PasswordError>`, то переменной `const combined = name.combineWith(password)` будет тип `Result<[Username, Password], (UsernameError | PasswordError)[]>`.  
Дальше если мы сделаем
```ts
combined.flatMap(([name, password]) => {
  return saveToDb(name, password);
});
```
, где `saveToDb` возвращает `Result<void, DatabaseError>`, то Typescript сам поймёт, что в результате получится тип `Result<void, (UsernameError | PasswordError)[] | DatabaseError>`.

Но с другой же стороны в Typescript никакия информация о discriminated union'ах не доходит до рантайма, поэтому нельзя сделать так же красиво как, например, в Kotlin:
```kotlin
when (something) {
    is A -> // do something
    is B -> // do something
    is C -> // do something
}
```
, а приходится городить всякие дискриминаторы
```ts
switch (something._t) {
    case "A":
        // do something
        break;
    case "B":
        // do something
        break;
    case "C":
        // do something
        break;
}
```

Typescript получает приз зрительских симпатий за экосистему и тулчейн. После того, как у вас появились тесты на `jest`, для того, чтобы начать считать coverage, вам достаточно добавить флаг `--collectCoverage` к вызову `jest`. В `npm` есть куча готового всего на все случаи жизни. Причём это работает на как spring-boot, где нельзя сделать шаг влево-шаг вправо, а просто как набор утилит, которые хорошо работают друг с другом.

Минус же, который я увидел в Typescript в рамках своего эксперимента не такой уж и значительный, относительно остальных языков - в Typescript нету ни pipe-оператора как в F# и bash, ни способа добавить метод расширения к любому типу, как в C# и Kotlin. Обе эти техники позволяют писать код, в котором данные текут естественно слева направо:
```
reader.ReadString().AsPassword().Hash() // C#
reader |> readString |> asPassword |> hash // F#
hash(asPassword(readString(reader))) // Typescript
```
В Typescript можно добавить метод расширения только для классов, а для типов, объявленных через ключевые слова `type` и `interface` нельзя.

Кстати в хаскелле пайп (`$`) и композиция (`.`) по-умолчанию работают тоже справа налево, но их довольно легко перевернуть.

## Промежуточные итоги

|   | Наличие DU | Когнитивная сложность | Поддержка IDE | Экосистема и тулчейн | Пайп/расширения |
|---|---|---|---|---|---|
| C# | ❌ | ✅ | ✅ | ✅ | ✅ |
| Haskell | ✅ | ❌ | ❌ | ✅ | ✅ |
| Kotlin | ✅ | ✅ | ✅ | ❌ | ✅ |
| Typescript | ✅ | ✅ | ✅ | ✅ | ❌ | 

🏆 Синтаксис - **Kotlin**
🏆 Экосистема и тулчейн - **Typescript**
