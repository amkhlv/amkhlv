const { mathjax } = require('mathjax-full/js/mathjax.js');
const { TeX } = require('mathjax-full/js/input/tex.js');
const { SVG } = require('mathjax-full/js/output/svg.js');
const { liteAdaptor } = require('mathjax-full/js/adaptors/liteAdaptor.js');
const { RegisterHTMLHandler } = require('mathjax-full/js/handlers/html.js');
const { AssistiveMmlHandler } = require('mathjax-full/js/a11y/assistive-mml.js');

const { AllPackages } = require('mathjax-full/js/input/tex/AllPackages.js');

const fs = require('node:fs');
const zmq = require('zeromq');
const homedir = require('os').homedir();

const {parseBibFile} = require("bibtex");

const ASSISTIVE_MML = false, FONT_CACHE = true, INLINE = false, packages = AllPackages.sort().join(', ').split(/\s*,\s*/);

const adaptor = liteAdaptor();
const handler = RegisterHTMLHandler(adaptor);
if (ASSISTIVE_MML) AssistiveMmlHandler(handler);

const tex = new TeX({ packages });
const svg = new SVG({ fontCache: (FONT_CACHE ? 'local' : 'none') });
const html = mathjax.document('', { InputJax: tex, OutputJax: svg });
const path = require('node:path');

const CSS = [
    'svg a{fill:blue;stroke:blue}',
    '[data-mml-node="merror"]>g{fill:red;stroke:red}',
    '[data-mml-node="merror"]>rect[data-background]{fill:yellow;stroke:none}',
    '[data-frame],[data-line]{stroke-width:70px;fill:none}',
    '.mjx-dashed{stroke-dasharray:140}',
    '.mjx-dotted{stroke-linecap:round;stroke-dasharray:0,140}',
    'use[data-c]{stroke-width:3px}'
].join('');

const sock_dir = path.join(homedir,".local", "run");
const bib_file = path.join(homedir, ".config", "bystrotex.bib");
if (!fs.existsSync(sock_dir)) {
  fs.mkdirSync(sock_dir);
}

function processBibField(field) {
  if (field === undefined) {return null}
  if (typeof field == "number") {
    return field
  } else {
    const fdata = field.data;
    return fdata.map(x => {try { return x.stringify() } catch (e) { return x.toString() }}).join(' ')    
  }
}

async function runServer() {
  const sock = new zmq.Reply();

  console.log(`Binding to ${path.join(sock_dir, "bystrotex.ipc")}`)
  await sock.bind(`ipc://${path.join(sock_dir, "bystrotex.ipc")}`);
  console.log(`Bound to ${path.join(sock_dir, "bystrotex.ipc")}`)

  for await (const [msg] of sock) {

    const obj = JSON.parse(msg);
    if ('texstring' in obj) {
      const node = html.convert(obj.texstring, {
          display: !INLINE,
      });
      const align = node.children[0].attributes.style;
      const width = node.children[0].attributes.width;
      const height = node.children[0].attributes.height;
      const dims = { valign : align, width : width, height: height };
      let svgString = adaptor.innerHTML(node);
      svgString = svgString.replace(/<defs>/, `<defs><style>${CSS}</style>`)

      fs.writeFile(obj.outpath, svgString, err => {
        if (err) {
          console.error(err);
        } else {
          // file written successfully
        }
      });
      await sock.send(JSON.stringify(dims));
    }
    if ('bibkey' in obj) {
      const bib = parseBibFile(fs.readFileSync(bib_file).toString());
      const entry = bib.getEntry(obj.bibkey);
      await sock.send(
        JSON.stringify(
          {year: processBibField(entry.getField("year")),
           journal: processBibField(entry.getField("journal")),
           volume: processBibField(entry.getField("volume")),
           pages: processBibField(entry.getField("pages")),
           title: processBibField(entry.getField("title")),
           author: processBibField(entry.getField("author")),
           }
        )
      );
    }
  }
}

runServer();
 
 

