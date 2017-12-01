const phantom = require("phantom");
const readlineSync = require("readline-sync");
const fs = require("fs");

const cheerio = require("cheerio");
(async function() {
  const instance = await phantom.create();
  const page = await instance.createPage();

  page.on("onConsoleMessage", function(msg) {
    console.log("CONSOLE:" + msg);
  });

  const status = await page.open("https://nettskjema.uio.no/answer/87831.html");

  let answer;
  while (answer !== "0") {
    await new Promise(resolve => {
      setTimeout(() => {
        resolve();
      }, 1000);
    });
    await renderHtml(page);
    answer = readlineSync.question("Command:");
    if (answer[0] === "C") {
      await page.evaluate(function(answer) {
        var elem = document.querySelectorAll(
          '[data-phantom-terminal-id="' + answer.slice(1) + '"]'
        );
        elem[0].click();
      }, answer);
    }
  }
  await instance.exit();
})();

async function enumeratePageElements(page) {
  await page.evaluate(function() {
    var all = document.getElementsByTagName("*");

    for (var i = 0, max = all.length; i < max; i++) {
      var elem = all[i];
      elem.setAttribute("data-phantom-terminal-id", i);
    }
  });
}

function filter($) {
  const scriptStuff = $("script, noscript");
  scriptStuff.remove();
}

function decorate($) {
  const clickables = $("a, button");
  clickables.each((i, elem) => {
    $(elem).prepend("[[C" + $(elem).attr("data-phantom-terminal-id") + "]]");
  });
  const textInputs = $("input[type=text], input[type=textarea]");
  textInputs.each((i, elem) => {
    $(elem).prepend("[[I" + $(elem).attr("data-phantom-terminal-id") + "]]");
  });
}

async function renderHtml(page) {
  await enumeratePageElements(page);
  const content = await page.property("content");
  const $ = cheerio.load(content);
  filter($);
  decorate($);
  fs.writeFileSync("browsed.html", $.html());
}
