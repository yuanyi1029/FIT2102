import { Viewport, Block } from "./constants";
import { isNotNullOrUndefined } from "./util";
import { SingleBlock, State } from "./types";

/**
 * translate a SingleBlock's x value to pixel value on the x axis
 * @param columnX x value to translate 
 */
const getPixelX = (columnX: number) => Block.WIDTH * columnX 

/**
 * translate a SingleBlock's y value to pixel value on the y axis
 * @param columnY y value to translate 
 */
const getPixelY = (columnY: number) => (Viewport.CANVAS_HEIGHT - Block.HEIGHT) - (Block.HEIGHT * columnY)

/**
 * Displays a SVG element on the canvas. Brings to foreground.
 * @param elem SVG element to display
 */
const show = (elem: SVGGraphicsElement) => {
  elem.setAttribute("visibility", "visible");
  elem.parentNode!.appendChild(elem);
};

/**
 * Hides a SVG element on the canvas.
 * @param elem SVG element to hide
 */
const hide = (elem: SVGGraphicsElement) => {
  elem.setAttribute("visibility", "hidden");
}

/**
 * Creates an SVG element with the given properties.
 * See https://developer.mozilla.org/en-US/docs/Web/SVG/Element for valid
 * element names and properties.
 * @param namespace Namespace of the SVG element
 * @param name SVGElement name
 * @param props Properties to set on the SVG element
 * @returns SVG element
 */
const createSvgElement = (
  namespace: string | null,
  name: string,
  props: Record<string, string> = {}
) => {
  const elem = document.createElementNS(namespace, name) as SVGElement;
  Object.entries(props).forEach(([k, v]) => elem.setAttribute(k, v));
  return elem;
};

/** Canvas Elements */
const svg = document.querySelector("#svgCanvas") as SVGGraphicsElement &
HTMLElement;
const preview = document.querySelector("#svgPreview") as SVGGraphicsElement &
HTMLElement;
const gameover = document.querySelector("#gameOver") as SVGGraphicsElement &
HTMLElement;
const container = document.querySelector("#main") as HTMLElement;

svg.setAttribute("height", `${Viewport.CANVAS_HEIGHT}`);
svg.setAttribute("width", `${Viewport.CANVAS_WIDTH}`);
preview.setAttribute("height", `${Viewport.PREVIEW_HEIGHT}`);
preview.setAttribute("width", `${Viewport.PREVIEW_WIDTH}`);

const levelText = document.querySelector("#levelText") as HTMLElement;
const scoreText = document.querySelector("#scoreText") as HTMLElement;
const highScoreText = document.querySelector("#highScoreText") as HTMLElement;

/**
 * Renders the current state to the canvas.
 * In MVC terms, this updates the View using the Model.
 * @param s Current state
 */
export const render = (s: State) => {
  // blocks to render (static + current tetromino)
  const viewBlocks = s.staticBlocks.concat(s.tetromino.blocks)
  
  // Remove preview blocks every tick
  Array.from(preview.childNodes).forEach(node => preview.removeChild(node))

  /**
   * Renders multiple blocks to a selected container (svg canvas / preview canvas)
   * @param blocks Array of SingleBlocks
   * @param container SVG element to render to
   */
  const renderBlocks = (blocks: ReadonlyArray<SingleBlock>, container: SVGGraphicsElement & HTMLElement) => {
    blocks.forEach(eachBlock => {
      const createBlockView = () => {
        const block = createSvgElement(svg.namespaceURI, "rect", {
          height: `${Block.HEIGHT}`,
          width: `${Block.WIDTH}`,
          x: "0",
          y: "0",
          style: `fill: ${eachBlock.colour}`,
          id: `${eachBlock.id}`
        });
        container.appendChild(block);
        return block;
      }
      let b = document.getElementById(`${eachBlock.id}`) || createBlockView();
      b.setAttribute('transform', `translate(${getPixelX(eachBlock.x)}, ${getPixelY(eachBlock.y)})`)
    })
  }

  // Render viewBlocks to svg canvas, and next tetromino blocks to preview canvas
  renderBlocks(viewBlocks, svg)
  renderBlocks(s.next.blocks, preview)

  // Remove exit blocks
  s.exitBlocks.map(eachBlock => document.getElementById(`${eachBlock.id}`)).filter(isNotNullOrUndefined).forEach(block => svg.removeChild(block))

  // Handle level text
  levelText.textContent = `${s.level}`;
  
  // Handle score text 
  scoreText.textContent = `${s.score}`;
  
  // Handle highscore text
  highScoreText.textContent = `${s.highscore}`;

  // Handle gameover 
  if (s.gameEnd) {
    show(gameover);
    // Remove all blocks when gameover
    Array.from(svg.childNodes).filter(node => node.nodeName === "rect").forEach(node => svg.removeChild(node))
    Array.from(preview.childNodes).forEach(node => preview.removeChild(node))
  } else {
    hide(gameover);
  }
};