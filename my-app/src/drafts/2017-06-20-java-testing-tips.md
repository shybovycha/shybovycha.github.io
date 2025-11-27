---- GET
tags: [rest, qa, java, testing, backend, programming, frontend, data, api, jvm]
---- //

    public <T> T getJSONOKResult(String urlPath, Class<T> responseType) {
        WebResource resource = resource(urlPath);
        try {
            return resource.accept(MediaType.APPLICATION_JSON_TYPE).get(responseType);
        } catch (UniformInterfaceException e) {
            throw new RuntimeException("Error when GETing JSON from '" + urlPath + "'", e);
        }
    }

    public <T> T getJSONErrorResult(String urlPath, int statusCode, Class<T> errorResponseType) {
        WebResource resource = resource(urlPath);
        try {
            final String responseBody = resource.accept(MediaType.APPLICATION_JSON_TYPE).get(String.class);
            throw new RuntimeException("Expected code " + statusCode + " but received 2xx. Body:\n" + responseBody);
        } catch (UniformInterfaceException uie) {
            return handleErrorResult(uie, statusCode, errorResponseType);
        }
    }

    // ---- POST ---- //

    public Either<ClientResponse, Unit> postJSONNoResponse(String urlPath, Object requestEntity) {
        WebResource resource = resource(urlPath);
        try {
            resource.type(MediaType.APPLICATION_JSON_TYPE).post(requestEntity);
            return right(Unit());
        } catch (UniformInterfaceException e) {
            log.debug("Error when POSTing JSON to '" + urlPath + "'", e);
            return left(e.getResponse());
        }
    }

    // ...
}

@JsonAutoDetect
public final class SomeResourceRestResponse {
    private final long id;
    private final String title;
    private final String text;

    @JsonCreator
    private CannedResponseRestResponse(
            @JsonProperty("id") long id,
            @JsonProperty("title") String title,
            @JsonProperty("text") String text) {
        this.id = id;
        this.title = title;
        this.text = text;
    }

    // standard bean methods - equals, toString, getters & setters
}

public class SomeResourceCreateRequest {
    private final String title;
    private final String text;

    @JsonCreator
    public CannedResponseCreateRequest(
            @JsonProperty("title") String title,
            @JsonProperty("text") String text) {
        this.title = title;
        this.text = text;
    }

    // ...
}

public class ErrorCollectionError {

    private String errorMessage;
    private Option<String> field;

    public ErrorCollectionError(@JsonProperty("errorMessage") String errorMessage,
                                @JsonProperty("field") String field) {
        this(errorMessage);
        this.field = option(field);
    }

    // ...
}

public class ErrorCollectionResponse {

    private List<ErrorCollectionError> errors;
    private String reasonKey;
    private String reasonCode;

    public ErrorCollectionResponse(@JsonProperty("errors") List<ErrorCollectionError> errors,
                                   @JsonProperty("reasonKey") String reasonKey,
                                   @JsonProperty("reasonCode") String reasonCode) {
        this.errors = errors;
        this.reasonKey = reasonKey;
        this.reasonCode = reasonCode;
    }

    // ...
}

public class SomeResourceClient extends RestClient {
    private static final String REST_PATH = "/rest/v1/some-resource/";

    public SomeResourceClient() {
        super(getBaseUrl().toExternalForm(), REST_PATH);
    }

    public SomeResourceRestResponse createResource(SomeResourceCreateRequest req) {
        return postJSONOKResult("/", req, SomeResourceRestResponse.class);
    }

    public ErrorCollectionResponse createResourceExpectingError(SomeResourceCreateRequest req,
                                                                int statusCode) {
        return postJSONErrorResult("/", req, statusCode, ErrorCollectionResponse.class);
    }

    // ...
}

@RunWith(MockitoJUnit4Runner.class)
public class SomeResourceTest {
    private static SomeResourceClient client;

    @BeforeClass
    public static void setUpClass() {
        client = new SomeResourceClient();
    }

    @Test
    public void test_createResource() throws Exception {
        SomeResourceCreateRequest request = new SomeResourceCreateRequest("title", "text");

        final SomeResourceRestResponse createResponse = client.createResource(request);

        assertThat("The returned response is ok", createResponse.getTitle(), is("title"));
    }
}
```

**Better:**

```java
// using RestAssured

@RunWith(MockitoJUnit4Runner.class)
public class SomeResourceTest {
    @Test
    public void test_createResource() throws Exception {
        given()
            .parameters("title", "title", "text", "text").
        .when()
            .post("/rest/v1/some-resource/").
        .then()
            .body("title", equalTo("title"));
    }
}
```
