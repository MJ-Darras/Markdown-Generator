import { from, fromEvent, merge } from "rxjs";
import { map, mergeScan, first, timestamp } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

const markdownInput = document.getElementById(
    "markdown-input",
) as HTMLTextAreaElement;
const checkbox = document.querySelector('input[name="checkbox"]')!;

const titleInput = document.getElementById("titleInput") as HTMLInputElement;
const saveButton = document.getElementById("saveButton")!;

type Action = (_: State) => State;

const resetState: Action = (s) => {
    return { ...s, save: false };
};

const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, markdown: value })),
);

const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((value) => (s) => ({ ...s, renderHTML: value })),
);

const titleInput$: Observable<Action> = fromEvent<InputEvent>(
    titleInput, 
    "input"
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({...s, title: value}))
);

const saveButtonClick$ = fromEvent(
    saveButton, 
    "click"
).pipe(
    map(() => (s: State) => ({...s, save: true}))
);

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: s.markdown,
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            return {
                ...s,
                HTML: data.html,
            };
        }),
        first(),
    );
}

function saveHTML(s: State): Observable<void> {
    const isoTime = new Date().toISOString();
    const data = {
        title: s.title,
        html: s.HTML,
        timestamp: isoTime
    };
    
    return ajax({
        url: "/api/saveHTML",
        method: "POST",
        headers: {
            "Content-Type" : "application/json"
        },
        body: JSON.stringify(data)
    }).pipe(map(() => {})); 
}

const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    save: false,
    title: ""
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(input$, checkboxStream$, titleInput$, saveButtonClick$)
        .pipe(
            map((reducer: Action) => {
                // Reset Some variables in the state in every tick
                const newReducer = compose(reducer)(resetState);
                return newReducer;
            }),
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                return getHTML(newState);
            }, initialState),
        )
        .subscribe((value) => {
            const htmlOutput = document.getElementById("html-output");
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                if (value.renderHTML) {
                    const highlight =
                        '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                    htmlOutput.innerHTML = highlight + value.HTML;
                    // Magic code to add code highlighting
                    const blocks = htmlOutput.querySelectorAll("pre code");
                    blocks.forEach((block) =>
                        hljs.highlightElement(block as HTMLElement),
                    );
                } else {
                    htmlOutput.textContent = value.HTML;
                }
            }
        });
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}
