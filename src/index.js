import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

let app = Elm.Main.init({
  node: document.getElementById("root"),
});

const START_DAY = 19147;
const GAME_NUMBER = (() => {
  const today = Math.floor(new Date().getTime() / 1000 / 60 / 60 / 24);
  return today - START_DAY;
})();

const LOCAL_STORAGE_KEY = "gameHistory";
const SAVE_FORMAT = "v1";

// written like trash but it's 4am and i need to get this done asap
app.ports.copyResultsToClipboard.subscribe(([board, isWin]) => {
  navigator.clipboard.writeText(
    `Pintordle ${GAME_NUMBER} ${isWin ? board.length : "x"}/6

${board.map((row) => row.join("")).join("\n")}
`
  );
});

/**
 * When the page loads, read the save data for the current day from local
 * storage and send it over to elm to populate the model
 */
window.addEventListener("DOMContentLoaded", () => {
  const gameHistory = JSON.parse(localStorage.getItem(LOCAL_STORAGE_KEY)) ?? [];
  const game = gameHistory.find((entry) => entry.gameNumber === GAME_NUMBER);
  const board = game?.board ?? [];
  const gameState = game?.gameState ?? "playing";
  app.ports.load.send([board, gameState]);
});

/**
 * When we receive a `save` command from Elm, store the board and the game
 * status (win/loss/playing) in localStorage
 */
app.ports.save.subscribe(([board, gameState]) => {
  let gameHistory = JSON.parse(localStorage.getItem(LOCAL_STORAGE_KEY)) ?? [];
  const gameIndex = gameHistory.findIndex(
    (entry) => entry.gameNumber === GAME_NUMBER
  );

  const newEntry = {
    format: SAVE_FORMAT,
    gameNumber: GAME_NUMBER,
    board,
    gameState,
  };

  if (gameIndex === -1) {
    gameHistory.push(newEntry);
  } else {
    gameHistory[gameIndex] = newEntry;
  }

  localStorage.setItem(LOCAL_STORAGE_KEY, JSON.stringify(gameHistory));
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
