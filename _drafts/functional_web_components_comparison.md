{% highlight haskell %}
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram { model = 0, view = view, update = update }

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
{% endhighlight %}


{% highlight ocaml %}
# ocamlc -dsource -ppx ../node_modules/reason/_build/src/reactjs_jsx_ppx.native -pp ../node_modules/reason/_build/src/refmt_impl.native -impl Test.re 2>&1 | sed '$ d' | sed '$ d' > Test.rsx && ../node_modules/reason/_build/src/refmt_impl.native -parse ml -print re -is-interface-pp false Test.rsx > TestOut.re

/* main =
  Html.beginnerProgram { model = 0, view = view, update = update } */

type Msg = Increment | Decrement;

let update = fun (msg, model) => {
  switch msg {
    | Increment => model + 1

    | Decrement => model - 1
  }
};

let view = fun (model) => {
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
};
{% endhighlight %}


{% highlight mustache %}
{% raw %}
<table>
  <thead>
    <tr>
      {{#each fields as field}}
        <th>{{field.title}}</th>
      {{/each}}
    </tr>
  </thead>

  <tbody>
    {{#each rows as row}}
    <tr>
      {{#each fields as field}}
      <td>{{row[field.name]}}</td>
      {{/each}}
    </tr>
    {{/each}}
  </tbody>
</table>

<script>
  export default {
    data() {
      return {
        fields: [
          { title: "Col #1", name: "col1" },
          { title: "Col #2", name: "col2" },
          { title: "Col ##3", name: "col3" }
        ],

        rows: [
          {col1: "val 1.1", col2: "val 1.2", col3: "val 1.3"},
          {col1: "val 2.1", col2: "val 2.2", col3: "val 2.3"},
        ]
      };
    }
  };
</script>
{% endraw %}
{% endhighlight %}
