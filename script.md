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
