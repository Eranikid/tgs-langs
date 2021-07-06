# Захотелось мне значит сделать формочку регистрации для веб приложения

Ну я взял свой любимый C# и сделал:
```c#
[ApiController]
[Route("/api/v1/[controller]")]
public class AccountController : ControllerBase {
    private readonly IDatabase _db;

    public AccountController(IDatabase db) {
        _db = db;
    }

    [HttpPost]
    public async Task<IActionResult> Create(CreateAccountRequest request) {
        await _db.Save(new {
            id = Guid.NewGuid(),
            name = request.Name,
            password = request.Password
        });

        return Ok();
    }
}
```

А потом подумал:
* А что если пароль слишком короткий или слишком длинный? Надо бы вернуть понятную ошибку
* А что если имя пользователя слишком короткое, слишком длинное, или содержит недопустимые символы? Надо бы вернуть понятную ошибку
* А если и то и другое невалидно, то надо бы вернуть обе ошибки, а не только первую случившуюся
* А что если имя пользователя уже занято и сохранение в БД упало с нарушением уникального constraint? Надо бы вернуть понятную ошибку
* А что если до базы не смогли достучаться? Надо вернуть бы 500 с понятной ошибкой
* Я что, зря всё это DDD читал? Чего у меня одни примитивы?

И переделал:
```c#
[ApiController]
[Route("/api/v1/[controller]")]
public class AccountController : ControllerBase {
    private readonly IDatabase _db;

    public AccountController(IDatabase db) {
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

        try {
            await _db.Save(new {
                id = Guid.NewGuid(),
                username,
                password
            });
        }
        catch (ConstraintViolationException) {
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
|   | ![csharp](https://user-images.githubusercontent.com/8256473/124615275-c7d78280-de7d-11eb-93de-8c8ee616579e.jpg) | 
|---|---|
| Pros |  |
| Cons | - Нет discriminated union'ов |

## Так и что делать-то теперь?

Ну, discriminated union'ы, это же функциональная фича, а какой у нас функциональный язык? Правильно, Haskell! Запасаемся мануалами и попробуем:

> Здесь была секция про Haskell, но поскольку есть риск потратить на неё очень много времени, я перенёс её в самый конец, а здесь оставил только выводы

Haskell решает нашу проблему, однако имеет свои проблемы:

1. Вполне ожидаемо, это когнитивная сложность и порог входа. Чистый функциональный язык заставляет вас начать думать по другому, а я не хочу размышлять в терминах теории категорий (по крайней мере я не нахожу целесообразным инвестировать время, чтобы научиться так размышлять). Верните мне мои переменные и оператор присваивания.
2. Поддержка IDE. Есть два конкурирующих language server'а (но один вроде как заобсолечен), и оба хуже чем поддержка JS в базовой конфигурации VS Code.
3. Экосистема и тулчейн. Опять два конкурирующих стандарта организации пакетов (но один вроде как расширяет другой), и оба хуже чем `pip`/`npm`/`cargo`/`gem`/`go`

## Промежуточные итоги
|   | ![csharp](https://user-images.githubusercontent.com/8256473/124615275-c7d78280-de7d-11eb-93de-8c8ee616579e.jpg) | ![haskell](https://user-images.githubusercontent.com/8256473/124616578-dffbd180-de7e-11eb-95be-83fc12e1836a.png) |
|---|---|---|
| Pros |  |  |
| Cons | - Нет discriminated union'ов | - Сложность<br/>- Инструментарий |

## А где же взять ЯП, с discriminated union'ами, хорошей поддержкой IDE, на котором можно писать без PhD по абстрактной алгебре?

Все слышали, а кто-то даже писал на Kotlin. Вы только посмотрите на страницу [код сниппетов](https://kotlinlang.org/docs/reference/idioms.html), там всё так интуитивно понятно, сразу ощущаешь что пишешь на языке созданном людьми для людей.

И бесплатная версия IntelliJ Idea от JetBrains буквально создана для Kotlin, так что с поддержкой IDE проблем не возникает. Давайте попробуем реализовать нашу задачку:
```kotlin
package mypackage

// Импортируем, чтобы можно было использовать `Ok` и `Err` без префикса `Result.`
import mypackage.Result.*

// Это будет класс, представляющий либо успешное выполнение задачи с результатом T, либо ошибку с описанием E
// Обратите внимание, что `sealed class` в Kotlin работает не так, как в C#
// Тут `sealed` означает, что все наследники класса известны в compile-time, что и даёт нам семантику discriminated union
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

Вещи, которые мы делаем MSBuild'ом или Cake'ом, в мире Kotlin делают при помощи Gradle. Gradle - это система сборки, работающая на JVM и использующая Kotlin в качестве билд скрипта (Можно ещё Groovy).

В темплейте проекта с Gradle и JUnit с юнит тестами предлагается работать так:
1. Пишешь тест, запускаешь прогон тестов, все проходят, в консоли написано что всё хорошо
2. Меняешь код, несколько тестов падают, в консоли тебе пишет что какие-то тесты упали, но какие именно и как не говорят
3. Ты открываешь браузером сгенерённый html-тест репорт, где написано сколько тестов упало
4. Переходишь на другую страничку сгенерённого html репорта, и смотришь какие именно тесты упали

Приведу ещё пример:

1. Я добавил как зависимость очень простой веб фреймворк в котором кроме роутинга ничего и нету
2. При запуске всё работало, но в консоли были ворнинги `SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder". SLF4J: Defaulting to no-operation (NOP) logger implementation`.
3. Как оказалось, в джаве DI работает через статический класс `ServiceLoader`, и мой фреймворк через этот статический класс пытается получить себе логгер (хотя его никто не просил, и DI ему не конфигурировал), а поскольку никаких зависимостей про логгинг я в свой проект не добавил, выплёвывается вот этот ворнинг.

Мне такой dev-experience не нравится, и в целом сильно чувствуется как протекает Java-legacy

Зато котлин получает от меня приз зрительских симпатий за синтаксис.

## Промежуточные итоги
|   | ![csharp](https://user-images.githubusercontent.com/8256473/124615275-c7d78280-de7d-11eb-93de-8c8ee616579e.jpg) | ![haskell](https://user-images.githubusercontent.com/8256473/124616578-dffbd180-de7e-11eb-95be-83fc12e1836a.png) | ![kotlin](https://user-images.githubusercontent.com/8256473/124619116-0884cb00-de81-11eb-9172-bd21957136c0.png) |
|---|---|---|---|
| Pros |  |  | + Синтаксис |
| Cons | - Нет discriminated union'ов | - Сложность<br/>- Инструментарий | - Инструментарий |

## Ну давайте уже наконец напишем на Typescript!

А давайте:
```ts
// Нам также понадобится свой Result, реализуем его как абстрактный класс
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

Т.е. если есть тип `type Animal = 'cat' | 'dog'` и тип `type Fruit = 'apple' | 'orange'`, то мы можем просто использовать тип `Animal | Fruit`, вместо того, чтобы создавать отдельный "зонтичный тип" `AnimalOrFruit`

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
В этом и заключается проблема TypeScript - он не даёт вам из коробки способа в рантайме отличить ветки discriminated union'а друг от друга. Хотя такое вполне себе есть в например в [ReScript](https://rescript-lang.org/). В итоге красиво на TypeScript тоже не получается.

TypeScript получает приз зрительских симпатий за экосистему и тулчейн. После того, как у вас появились тесты на `jest`, для того, чтобы начать считать coverage, вам достаточно добавить флаг `--collectCoverage` к вызову `jest`. В `npm` есть куча готового всего на все случаи жизни. Причём это работает не как spring-boot, где нельзя сделать шаг влево-шаг вправо, а просто как набор библиотек, которые хорошо работают друг с другом.

## Промежуточные итоги

|   | ![csharp](https://user-images.githubusercontent.com/8256473/124615275-c7d78280-de7d-11eb-93de-8c8ee616579e.jpg) | ![haskell](https://user-images.githubusercontent.com/8256473/124616578-dffbd180-de7e-11eb-95be-83fc12e1836a.png) | ![kotlin](https://user-images.githubusercontent.com/8256473/124619116-0884cb00-de81-11eb-9172-bd21957136c0.png) | ![typescript](https://user-images.githubusercontent.com/8256473/124620974-92816380-de82-11eb-9404-ecb063881f79.png) |
|---|---|---|---|---|
| Pros |  |  | + Синтаксис | + Инструментарий |
| Cons | - Нет discriminated union'ов | - Сложность<br/>- Инструментарий | - Инструментарий | - Нет discriminated union'ов в рантайме |

## We need to go deeper

В поисках того, чтобы мне ещё попробовать, я забрёл в [результаты опроса StackOverflow за 2020 год](https://insights.stackoverflow.com/survey/2020#most-loved-dreaded-and-wanted), а там на первом месте в Most loved languages стоит Rust. Инсталлируем и пробуем:

```rust
// Эту функцию видим не первый раз, а тип Result уже есть в стандартной библиотеке
fn combine_results<T1, T2, E>(r1: Result<T1, E>, r2: Result<T2, E>) -> Result<(T1, T2), Vec<E>> {
    match (r1, r2) {
        (Ok(v1), Ok(v2)) => Ok((v1, v2)),
        (Ok(_), Err(e2)) => Err(vec![e2]),
        (Err(e1), Ok(_)) => Err(vec![e1]),
        (Err(e1), Err(e2)) => Err(vec![e1, e2]),
    }
}

// Это всё тоже видели
enum UsernameError {
    TooShort,
    TooLong,
}

struct Username {
    value: String,
}

// В Rust методы отдельно, данные отдельно
impl Username {
    fn new(value: String) -> Result<Self, UsernameError> {
        match value.chars().count() {
            len if len < 3 => Err(UsernameError::TooShort),
            len if len > 25 => Err(UsernameError::TooLong),
            _ => Ok(Username { value }),
        }
    }
}

enum PasswordError {
    TooShort,
    TooLong,
}

struct Password {
    value: String,
}

impl Password {
    fn new(value: String) -> Result<Self, PasswordError> {
        match value.chars().count() {
            len if len < 3 => Err(PasswordError::TooShort),
            len if len > 25 => Err(PasswordError::TooLong),
            _ => Ok(Password { value }),
        }
    }
}

enum DatabaseError {
    UsernameTaken,
    UnexpectedError,
}

fn save_to_db(name: Username, password: Password) -> Result<(), DatabaseError> {
    Ok(())
}

// Общий тип ошибки
enum HandlerError {
    UsernameError(UsernameError),
    PasswordError(PasswordError),
    DatabaseError(DatabaseError),
}

// Учим частные ошибки превращаться в общий тип
impl From<UsernameError> for HandlerError {
    fn from(e: UsernameError) -> Self {
        HandlerError::UsernameError(e)
    }
}

impl From<PasswordError> for HandlerError {
    fn from(e: PasswordError) -> Self {
        HandlerError::PasswordError(e)
    }
}

impl From<DatabaseError> for Vec<HandlerError> {
    fn from(e: DatabaseError) -> Self {
        vec![HandlerError::DatabaseError(e)]
    }
}

// Главная функция
fn createAccount(name: String, password: String) -> Result<(), Vec<HandlerError>> {
    let name = Username::new(name).map_err(HandlerError::from);
    let password = Password::new(password).map_err(HandlerError::from);

    let (name, password) = combine_results(name, password)?;

    Ok(save_to_db(name, password)?)
}
```

В Rust я нашёл для себя много разных крутых штук:
* Полиморфизм на трейтах
* Система овнершипа как замена `GC`/`malloc`
* Оператор `?` позволяющий развернуть значение из `Result<T, E>` или в `Option<T>` если оно там есть, или же сразу сделать возврат из containing метода если там ошибка и containing метод тоже возвращает такой же тип
* Исполняемые тесты из примеров в документации
* В других языках при вызове функций с дженерик параметрами, компилятор либо заинферит их все сам, либо, если компилятор не может заинферить хотя бы один, то пользователю придётся указать все. В Rust же если компилятор не может заинферить только один из типов, то можно указать только его, а остальные забить плейсхолдером `_` "Выведи сам". И в целом вывод типов в Rust очень понравился

Язык очень круто, но я споткнулся и в Rust, когда попытался углубиться.

Мне нужна была функция, которая примет асинхронный делегат, который в свою очередь тоже примет асихнронный делегат.

В C# вот так:
```csharp
public Task MyFunction(Func<Func<String, Int32, Task>, Task> body);
```

В Rust я долго пытался этого добиться и долго ловил непонятные ошибки из разряда `parameter body has unknown size at compile time` или `type TArg does not implement Send trait`, пока не пошёл на форум и мне не помогли:

```rust
type BoxedFn = Box<dyn FnOnce(String, i32) -> BoxFuture<'static, bool>>;

pub async fn test_helper<F, Fut>(test_body: F) -> ()
where
    F: FnOnce(BoxedFn) -> Fut,
    Fut: Future<Output = bool>
```

Все вот эти слова `Box`, `dyn`, `BoxFuture` и `'static` написаны не просто так и влияют на то, где и как будут аллоцированы объекты, сколько они будут жить, и т.д.
Но мне было неудобно думать о стэке, куче и лайфтаймах - мне удобно думать про асинхронную функцию, которая принимает асинхронную функцию, которая принимает асинхронную функцию. Поэтому проблемой Rust для меня так же становится когнитивная сложность.

По остальным параметрам:  
* **Поддержка IDE** - С Rust уже вполне себе приятно работать, а будущий релиз 2021 ставит своей целью прокачать поддержку IDE ещё лучше
* **Экосистема и тулчейн** - cargo и crates.io работают отлично
* **Пайп и расширения** - расширения можно, но чуть сложнее чем в C#

И конечно Rust получает приз зрительских симпатий в номинации быстродействие

## Итоги

|   | Наличие DU | Когнитивная сложность | Поддержка IDE | Экосистема и тулчейн | Пайп/расширения |
|---|---|---|---|---|---|
| C# | ❌ | ✅ | ✅ | ✅ | ✅ |
| Haskell | ✅ | ❌ | ❌ | ✅ | ✅ |
| Kotlin | ✅ | ✅ | ✅ | ❌ | ✅ |
| Typescript | ✅ | ✅ | ✅ | ✅ | ❌ | 
| Rust | ✅ | ❌ | ✅ | ✅ | ✅ |

🏆 Синтаксис - **Kotlin**  
🏆 Экосистема и тулчейн - **Typescript**  
🏆 Быстродействие - **Rust**

* Ждём, пока в C# заимплементят пропозал по discriminated union'ам
* Ждём, пока кто нибудь не придумает как компилировать Kotlin мимо Gradle (сохранив при этом все его сильные стороны)
* Ждём, пока в Typescript примут пропозал на методы расширения, или когда в ECMAscript примут пропозал на пайп оператор, хоть этот минус на фоне остальных языков выглядит самым незначительным.
* Ждём, пока кто нибудь не придумает Rust Virtual Machine, чтобы иметь возможность разменять перформанс на код попроще

Если кто-то знает какой-то язык, который меня может заинтересовать, то пожалуйста предлагайте

## Код на Haskell

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
