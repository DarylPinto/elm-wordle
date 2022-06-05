import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

let app = Elm.Main.init({
  node: document.getElementById('root')
});

// written like trash but it's 4am and i need to get this done asap
app.ports.copyResultsToClipboard.subscribe(([board, isWin]) => {
  const START_DAY = 19147;
  const today = Math.floor(new Date().getTime() / 1000 / 60 / 60 / 24);
  const gameNumber = today - START_DAY;

  navigator.clipboard.writeText(
`Pintordle ${gameNumber} ${isWin ? board.length : 'x'}/6

${board.map(row => row.join("")).join("\n")}
`
  );
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
