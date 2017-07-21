# Java tips

## Testing

## Running a single test

{% highlight java %}
import org.junit.runner.JUnitCore;
import org.junit.runner.Request;
import org.junit.runner.Result;

public class SingleJUnitTestRunner {
    public static void main(String... args) throws ClassNotFoundException {
        String[] classAndMethod = args[0].split("#");
        Request request = Request.method(Class.forName(classAndMethod[0]),
                classAndMethod[1]);

        Result result = new JUnitCore().run(request);
        System.exit(result.wasSuccessful() ? 0 : 1);
    }
}
{% endhighlight %}

And then run `java -cp path/to/testclasses:path/to/junit-4.8.2.jar SingleJUnitTestRunner com.mycompany.product.MyTestClass#testMethod`

### Returning different results on mock calls

{% highlight java %}
@RunWith(MockitoJUnitRunner.class)
public class ProjectManagerTest {
    @Mock
    private DatabaseConnection databaseConnection;

    @Mock
    private ProjectManager projectManager;

    @Mock
    private Project project;

    @Mock
    private SystemError error;

    @Before
    public void setUp() {
        // ProjectManager::getProject(DatabaseConnection conn, Long id) -> Project

        // Very bad:
        when(projectManager.getProject(databaseConnection, any(Long.class))).thenAnswer(invocation -> {
            if (invocation.getArgumentAt(1, Long.class) == projectId)
                return right(project); else
                    return left(error);
        });

        // Better:
        when(projectManager.getProject(databaseConnection, validProjectId)).thenReturn(right(project));

        when(projectManager.getProject(databaseConnection, invalidProjectId)).thenReturn(right(project));
    }
}
{% endhighlight %}