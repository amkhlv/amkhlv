const { mathjax } = require('mathjax-full/js/mathjax.js');
const { TeX } = require('mathjax-full/js/input/tex.js');
const { SVG } = require('mathjax-full/js/output/svg.js');
const { liteAdaptor } = require('mathjax-full/js/adaptors/liteAdaptor.js');
const { RegisterHTMLHandler } = require('mathjax-full/js/handlers/html.js');
const { AssistiveMmlHandler } = require('mathjax-full/js/a11y/assistive-mml.js');

const { AllPackages } = require('mathjax-full/js/input/tex/AllPackages.js');

const fs = require('node:fs');
const zmq = require('zeromq');
const ASSISTIVE_MML = false, FONT_CACHE = true, INLINE = false, packages = AllPackages.sort().join(', ').split(/\s*,\s*/);

const adaptor = liteAdaptor();
const handler = RegisterHTMLHandler(adaptor);
if (ASSISTIVE_MML) AssistiveMmlHandler(handler);

const tex = new TeX({ packages });
const svg = new SVG({ fontCache: (FONT_CACHE ? 'local' : 'none') });
const html = mathjax.document('', { InputJax: tex, OutputJax: svg });

const CSS = [
    'svg a{fill:blue;stroke:blue}',
    '[data-mml-node="merror"]>g{fill:red;stroke:red}',
    '[data-mml-node="merror"]>rect[data-background]{fill:yellow;stroke:none}',
    '[data-frame],[data-line]{stroke-width:70px;fill:none}',
    '.mjx-dashed{stroke-dasharray:140}',
    '.mjx-dotted{stroke-linecap:round;stroke-dasharray:0,140}',
    'use[data-c]{stroke-width:3px}'
].join('');
async function runServer() {
  const sock = new zmq.Reply();

  await sock.bind('ipc:///home/andrei/.local/run/bystrotex.ipc');

  for await (const [msg] of sock) {
    console.log('Received ' + ': ' + msg.toString() );
    const obj = JSON.parse(msg);
    console.log('Equation ' + ': ' + obj.texstring);

    const node = html.convert(obj.texstring, {
        display: !INLINE,
        em: 20 * obj.size,
        ex: 10 * obj.size,
        scale: obj.size
    });

    const svgWidth = node.children[0].attributes.viewBox.split(' ')[2];

    const align = node.children[0].attributes.style;
    const width = node.children[0].attributes.width;
    const height = node.children[0].attributes.height;
    const style = `${align} width: ${width}; height: ${height}`;
    let svgString = adaptor.innerHTML(node);
    svgString = svgString.replace(/<defs>/, `<defs><style>${CSS}</style>`)
    

    fs.writeFile(obj.outpath, svgString, err => {
      if (err) {
        console.error(err);
      } else {
        // file written successfully
      }
    });
    await sock.send(style);
  }
}

runServer();
 
 

