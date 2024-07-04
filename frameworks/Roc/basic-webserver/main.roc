app [main] {
    pf: platform "https://github.com/roc-lang/basic-webserver/releases/download/0.4.0/iAiYpbs5zdVB75golcg_YMtgexN3e2fwhsYPLPCeGzk.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.0/KbIfTNbxShRX1A1FgXei1SpO5Jn8sgP6HP6PXbi-xyA.tar.br",
}


import json.Json
import pf.Task exposing [Task]
import pf.Http exposing [Request, Response, defaultRequest, methodToStr]
import pf.SQLite3
#import pf.Utc
#import pf.Stdout

sqlitePath = "main.db"


############
### Main ###
############


main : Request -> Task Response []
main = processRequest myApp


myApp : App
myApp =
    { router : \req ->
        when (req.method, urlSegments req.url) is
            (Get, ["json", ..]) -> jsonHandler
            (Get, ["db", ..]) -> singleQueryHandler
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
    postprocessResponse : Response -> Task Response []
}
Router : Request -> Handler
App : {
  router : Router,
  middleware : List Middleware
}


preprocess : Task Request [], Middleware -> Task Request []
preprocess = \req, middleware -> Task.await req middleware.preprocessRequest
        

postprocess : Task Response [], Middleware -> Task Response []
postprocess = \resp, middleware -> Task.await resp middleware.postprocessResponse


handleRequest : Task Request [], Router -> Task Response []
handleRequest = \reqTask, myRouter ->
    Task.await reqTask \req ->
        handler = myRouter req
        handler req


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
    Task.ok request
        #|> \req -> List.walk app.middleware req preprocess
        |> handleRequest app.router
        |> \req -> List.walkBackwards app.middleware req postprocess


noopMiddleware : Middleware
noopMiddleware = {
    preprocessRequest: \req -> Task.ok req,
    postprocessResponse: \resp -> Task.ok resp,
}


urlSegments : Str -> List Str
urlSegments = \url ->
    when Str.split url "/" is
        [] -> []
        ["", .. as rest] -> rest
        other -> other


############
### JSON ###
############


encodeJson : a -> List U8 where a implements Encoding
encodeJson = \obj ->
    Encode.toBytes obj Json.utf8


jsonHandler : Handler
jsonHandler = \_req ->
    Task.ok {
        status: 200,
        headers: [{ name: "Content-Type", value: Str.toUtf8 "application/json" }],
        #body: Str.toUtf8 "{ \"message\": \"Hello, World!\" }",
        body: encodeJson { message : "Hello, World!" }
    }


#############################
### Single Database Query ###
#############################


singleQueryHandler : Handler
singleQueryHandler = \_req ->
    #Sqlite.execute {
    #    path: sqlitePath,
    #    query : "SELECT * FROM World WHERE id = abs(random() % 10000);",
    #    bindings: [{name: "id", value: Num.toStr id}]
    #}
    #|> Task.map \rows ->
    #    when rows is
    #        [] -> {
    #            status: 404,
    #            headers: [],
    #            body: Str.toUtf8 "404 Not Found"
    #        }
    #        [[Integer id, Integer num]] -> {
    #            status: 200,
    #            headers: [
    #                { name: "Content-Type", value: Str.toUtf8 "application/json" },
    #                { name: "Server", value: Str.toUtf8 "roc-basic-webserver" },
    #                { name: "Date", value: Str.toUtf8 "Sun, 10 Oct 2021 14:00:00 GMT" },
    #            ],
    #            body: encodeJson { id : id, randomNumber : num }
    #        }
    #        _ -> {
    #            status: 500,
    #            headers: [],
    #            body: Str.toUtf8 "500 Internal Server Error\n\nUnexpected response from database"
    #        }
    #|> Task.onErr \err ->
    #    Ok {
    #        status: 500,
    #        headers: [],
    #        body: Str.toUtf8 "500 Internal Server Error\n\n$(SQLite3.errToStr err)"
    #    }
    #|> Task.await
    Task.ok { status: 200, headers: [], body: Str.toUtf8 "Hello, World!" }
