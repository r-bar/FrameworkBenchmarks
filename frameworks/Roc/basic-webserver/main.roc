app [main] {
    pf: platform "https://github.com/roc-lang/basic-webserver/releases/download/0.4.0/iAiYpbs5zdVB75golcg_YMtgexN3e2fwhsYPLPCeGzk.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.0/KbIfTNbxShRX1A1FgXei1SpO5Jn8sgP6HP6PXbi-xyA.tar.br",
}


import json.Json
import pf.Task exposing [Task]
import pf.Http exposing [Request, Response, methodToStr]
import pf.SQLite3
import pf.Url
#import pf.Utc
import pf.Stdout

sqlitePath = "main.db"


############
### Main ###
############


main : Request -> Task Response []
main = processRequest myApp


myApp : App
myApp =
    { router : \req ->
        when (req.method, pathSegments req.url) is
            (Get, ["json", ..]) -> jsonHandler
            (Get, ["db", ..]) -> singleQueryHandler
            (Get, ["queries", ..]) -> multipleQueryHandler
            (Get, ["fortunes", ..]) -> fortunesHandler
            (Get, ["updates", ..]) -> updateQueriesHandler
            (Get, ["plaintext", ..]) -> plaintextHandler
            _ -> notFoundHandler
    , middleware : [standardHeaders]
    }


standardHeaders : Middleware
standardHeaders = {
    noopMiddleware &
    postprocessResponse: \response ->
        #TODO: real date generation
        #date = Task.ok! "Sun, 10 Oct 2021 14:00:00 GMT"
        headers = List.concat response.headers [
            { name: "Server", value: Str.toUtf8 "roc-basic-webserver" },
            #{ name: "Date", value: Str.toUtf8 date },
            { name: "Date", value: Str.toUtf8 "Sun, 10 Oct 2021 14:00:00 GMT" },
        ]
        Task.ok { status: response.status, body: response.body, headers }
}


notFoundHandler : Handler
notFoundHandler = \req ->
    body = Str.toUtf8 "404 Not Found\n$(methodToStr req.method) $(req.url)"
    Task.ok { status: 404, headers: [], body }



#################
### Framework ###
#################

Handler : Request -> Task Response []
Middleware : {
    preprocessRequest : Request -> Task Request [],
    postprocessResponse : Response -> Task Response [],
}
Router : Request -> Handler
App : {
  router : Router,
  middleware : List Middleware,
}


## Given middleware [a, b, c], the request will be processed as follows:
##
##   a.preprocessRequest
##   b.preprocessRequest
##   c.preprocessRequest
##   router request
##   c.postprocessResponse
##   b.postprocessResponse
##   a.postprocessResponse
processRequest : App -> (Request -> Task Response [])
processRequest = \app -> \request ->
    #datetime = Task.map! Utc.toIso8601Str Utc.now
    #Stdout.line! "$(datetime) $(Http.methodToStr req.method) $(req.url)"
    #preprocess = \req, middleware -> Task.await req middleware.preprocessRequest
    postprocess = \resp, middleware -> Task.await resp middleware.postprocessResponse
    handleRequest = \reqTask ->
        Task.await reqTask \req ->
            handler = app.router req
            handler req
    Task.ok request
        #|> \req -> List.walk app.middleware req preprocess
        |> handleRequest
        |> \req -> List.walkBackwards app.middleware req postprocess


noopMiddleware : Middleware
noopMiddleware = {
    preprocessRequest: \req -> Task.ok req,
    postprocessResponse: \resp -> Task.ok resp,
}


pathSegments : Str -> List Str
pathSegments = \urlStr ->
    url = Url.fromStr urlStr
    when Str.split (Url.path url) "/" is
        [] -> []
        ["", .. as rest] -> rest
        other -> other


encodeJson : a -> List U8 where a implements Encoding
encodeJson = \obj ->
    Encode.toBytes obj Json.utf8


jsonResponse : a, U16 -> Response where a implements Encoding
jsonResponse = \body, status ->
    { status
    , headers: [{ name: "Content-Type" , value: Str.toUtf8 "application/json" }]
    , body: encodeJson body
    }


textResponse : Str, U16 -> Response
textResponse = \body, status ->
    { status
    , headers: [{ name: "Content-Type" , value: Str.toUtf8 "text/plain; charset=UTF-8" }]
    , body: Str.toUtf8 body
    }

htmlResponse : Str, U16 -> Response
htmlResponse = \body, status ->
    { status
    , headers: [{ name: "Content-Type" , value: Str.toUtf8 "text/html; charset=UTF-8" }]
    , body: Str.toUtf8 body
    }


withErrorHandling :
    # handler                     # error handler
    (Request -> Task Response a), (a -> Task Response []) -> Handler
withErrorHandling = \handler, errorHandler -> \request ->
    handler request
      |> Task.onErr errorHandler


batch : List (Task a e) -> Task (List a) e
batch = \tasks ->
    List.walk tasks (Task.ok []) \accTask, task ->
        Task.await accTask \acc ->
            Task.await task \result ->
                List.append acc result
                |> Task.ok


htmlEscape : Str -> Str
htmlEscape = \str ->
    str
        |> Str.replaceEach "&" "&amp;"
        |> Str.replaceEach "<" "&lt;"
        |> Str.replaceEach ">" "&gt;"
        |> Str.replaceEach "\"" "&quot;"
        |> Str.replaceEach "'" "&#39;"
        |> Str.replaceEach "/" "&#x2F;"
        |> Str.replaceEach "\\" "&#x5C;"



############
### JSON ###
############


jsonHandler : Handler
jsonHandler = \_req ->
    jsonResponse { message : "Hello, World!" } 200
        |> Task.ok


#############################
### Single Database Query ###
#############################


singleQueryHandler : Handler
singleQueryHandler = \_req ->
    SQLite3.execute
        { path: sqlitePath
        , query : "SELECT * FROM World WHERE id = abs(random()) % 10000;"
        , bindings: []
        }
    |> Task.map \rows ->
        when rows is
            [] ->
                textResponse "404 Not Found" 404
            [[Integer id, Integer randomNumber]] ->
                jsonResponse { id, randomNumber } 200
            _ ->
                textResponse "Unexpected response from database" 500
    |> Task.onErr \err ->
        SQLite3.errToStr err
            |> textResponse 500
            |> Task.ok


########################
### Multiple Queries ###
########################


multipleQuery : U64 -> Task (List { id: I64, randomNumber: I64 }) [SQLError _ _, UnexpectedDatabaseResponse]
multipleQuery = \count ->
    queries = List.repeat "SELECT * FROM World WHERE id = abs(random()) % 10000;" count
    List.walk queries (Task.ok []) \resultAcc, query ->
        Task.await resultAcc \acc ->
            SQLite3.execute { path: sqlitePath, query, bindings: [] }
                |> Task.await \rows ->
                    when rows is
                        [[Integer id, Integer randomNumber]] ->
                            List.append acc { id, randomNumber }
                            |> Task.ok
                        _ -> Task.err UnexpectedDatabaseResponse


multipleQueryHandler : Handler
multipleQueryHandler =
    withErrorHandling
        \req ->
            #Stdout.line! "Request: $(req.url)"
            count = Url.queryParams (Url.fromStr req.url)
                |> Dict.get "queries"
                |> Result.try Str.toU64
                |> Task.fromResult!
                |> Num.min 500
            rows = multipleQuery! count
            jsonResponse rows 200
                |> Task.ok
        \err ->
            when err is
                InvalidNumStr ->
                  textResponse "Invalid number of queries" 400
                      |> Task.ok
                KeyNotFound ->
                  textResponse "404 Not Found" 404
                      |> Task.ok
                UnexpectedDatabaseResponse ->
                  textResponse "Unexpected response from database" 500
                      |> Task.ok
                SQLError _dbCode _dbErr ->
                  textResponse "Database error" 500
                      |> Task.ok


################
### Fortunes ###
################


strCmp : Str, Str -> [GT, EQ, LT]
strCmp = \astr, bstr ->
    cmp = \aBytes, bBytes ->
        when (aBytes, bBytes) is
            ([], []) -> EQ
            ([], _) -> LT
            (_, []) -> GT
            ([a, .. as aRest], [b, .. as bRest]) ->
                when Num.compare a b is
                    EQ -> cmp aRest bRest
                    LT -> LT
                    GT -> GT
    cmp (Str.toUtf8 astr) (Str.toUtf8 bstr)


Fortune : { id: I64, message: Str }


fortunesListQuery : Task (List Fortune) [SQLError _ _, UnexpectedDatabaseResponse]
fortunesListQuery = 
    SQLite3.execute! { path: sqlitePath, query: "SELECT * FROM Fortune;", bindings: [] }
        |> List.map \row ->
            when row is
                [Integer id, Str message] -> Task.ok { id, message }
                _ -> Task.err UnexpectedDatabaseResponse
        |> batch


dynamicfortunes : List Fortune -> List Fortune
dynamicfortunes = \fortunes ->
    List.append fortunes { id: 0, message: "Additional fortune added at request time." }
        |> List.sortWith \a, b -> strCmp a.message b.message


fortunesHandler : Handler
fortunesHandler =
    withErrorHandling
        \_req -> 
            fortunes = dynamicfortunes fortunesListQuery!
            htmlTableRows = List.walk fortunes "" \acc, { id, message } ->
                "$(acc)\n<tr><td>$(Num.toStr id)</td><td>$(htmlEscape message)</td></tr>"
            fortunesPage =
                """
                <!DOCTYPE html>
                <html>
                <head><title>Fortunes</title></head>
                <body>
                <table>
                <tr><th>id</th><th>message</th></tr>
                $(htmlTableRows)
                </table>
                </body>
                </html>
                """
            htmlResponse fortunesPage 200
                |> Task.ok
        \err ->
            when err is
                UnexpectedDatabaseResponse ->
                  textResponse "Unexpected response from database" 500
                      |> Task.ok
                SQLError _dbCode _dbErr ->
                    textResponse "Database error" 500
                        |> Task.ok



###############
### Updates ###
###############



updateQuery : List U64 -> Task {} _
updateQuery = \ids ->
    query = List.map ids \id ->
            "UPDATE World SET randomNumber = abs(random()) % 10000 WHERE id = $(Num.toStr id);"
        |> List.prepend "BEGIN;"
        |> List.append "COMMIT;"
        |> Str.joinWith "\n"
    _ = SQLite3.execute! { path: sqlitePath, query, bindings: [] }
    Task.ok {}


updateQueriesHandler : Handler
updateQueriesHandler =
    withErrorHandling
        \req ->
            count : U64
            count = Url.queryParams (Url.fromStr req.url)
                |> Dict.get "queries"
                |> Result.try Str.toU64
                |> Task.fromResult!
                |> Num.min 500
            rows = multipleQuery! count
            updateQuery! (List.map rows \{ id } -> Num.toU64 id)
            jsonResponse rows 200
                |> Task.ok
        \err ->
            when err is
                InvalidNumStr ->
                  textResponse "Invalid number of queries" 400
                      |> Task.ok
                KeyNotFound ->
                  textResponse "\"queries\" parameter not found" 404
                      |> Task.ok
                SQLError _dbCode _dbErr ->
                    textResponse "Database error" 500
                        |> Task.ok
                UnexpectedDatabaseResponse ->
                    textResponse "Unexpected response from database" 500
                        |> Task.ok


#################
### Plaintext ###
#################


plaintextHandler : Handler
plaintextHandler = \_req ->
    textResponse "Hello, World!" 200
        |> Task.ok
