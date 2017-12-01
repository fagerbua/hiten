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
    await enumeratePageElements(page);
    const content = await page.property("content");
    const $ = cheerio.load(content);
    filter($);
    fs.writeFileSync("browsed.html", $.html());
    answer = readlineSync.question(
      "Follow link? (0 to exit, p to print links)"
    );
    if (answer === "p") {
      await printLinks($);
    } else {
      await page.evaluate(function(answer) {
        var elem = document.querySelectorAll(
          '[data-phantom-terminal-id="' + answer + '"]'
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
  console.warn("SCRIPTS", scriptStuff);
  scriptStuff.remove();
}

function printLinks($) {
  const links = $("button, a");
  links.each((i, elem) => {
    console.log(
      "[" + $(elem).attr("data-phantom-terminal-id") + "]",
      $(elem).html()
    );
  });
}
