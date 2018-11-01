import './main.css';
import {Elm} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root'),
});

let svg = document.getElementsByTagName('svg')[0];
svg.addEventListener('mousemove', e => {
  let targetElement = e.target;
  if (!targetElement.ownerSVGElement) return;

  let point = targetElement.ownerSVGElement.createSVGPoint();
  point.x = e.clientX;
  point.y = e.clientY;
  let position = point.matrixTransform(targetElement.getScreenCTM().inverse());

  let event = new CustomEvent('mousemoveWithCoordinates', {
    detail: {
      x: position.x,
      y: position.y,
    },
  });
  targetElement.dispatchEvent(event);
});

registerServiceWorker();
