const phantom = require("phantom");
const readlineSync = require("readline-sync");

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
    await visitPage(page);
    answer = readlineSync.question(
      "Follow link? (0 to exit, p to print links)"
    );
    if (answer === "p") {
      await printLinks(page);
    } else {
      await page.evaluate(function(answer) {
        var elem = document.querySelectorAll(
          '[data-phantom-terminal-id="' + answer + '"]'
        );
        console.log("!!!", elem);
        elem[0].click();
      }, answer);
    }
  }

  await instance.exit();
})();

async function visitPage(page) {
  await page.evaluate(function() {
    var all = document.getElementsByTagName("*");

    for (var i = 0, max = all.length; i < max; i++) {
      var elem = all[i];
      elem.setAttribute("data-phantom-terminal-id", i);
    }
    console.log("set attributes");
  });
}

async function printLinks(page) {
  const content = await page.property("content");

  const $ = cheerio.load(content);
  const links = $("button");
  links.each((i, elem) => {
    console.log(
      "[" + $(elem).attr("data-phantom-terminal-id") + "]",
      $(elem).html()
    );
  });
  console.log("DONE VISITPAGE");
}
