const webdriverio = require("webdriverio");
const readlineSync = require("readline-sync");
const fs = require("fs");
const cheerio = require("cheerio");

const CHROME_BIN_PATH =
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";

const options = {
  desiredCapabilities: {
    browserName: "chrome",
    chromeOptions: {
      binary: CHROME_BIN_PATH,
      args: [
        "headless",
        // Use --disable-gpu to avoid an error from a missing Mesa
        // library, as per
        // https://chromium.googlesource.com/chromium/src/+/lkgr/headless/README.md
        "disable-gpu"
      ]
    }
  }
};

(async function() {
  const browser = webdriverio
    .remote(options)
    .init()
    .url("https://nettskjema.uio.no/answer/87831.html");

  let answer;
  while (answer !== "0") {
    await renderHtml(browser);
    answer = readlineSync.question("Command:");
    if (answer[0] === "C") {
      await browser.click(
        '[data-phantom-terminal-id="' + answer.slice(1) + '"]'
      );
    }
    if (answer[0] === "I") {
      const inputData = answer.slice(answer.indexOf("=") + 1);
      const selector =
        '[data-phantom-terminal-id="' +
        answer.slice(1, answer.indexOf("=")) +
        '"]';
      console.log(inputData, selector);
      await browser.click(selector);
      await browser.setValue(selector, inputData);
    }
    if (answer[0] === "R") {
      await renderHtml(browser);
    }
  }
})();

async function enumeratePageElements(browser) {
  await browser.execute(function() {
    var all = document.getElementsByTagName("*");

    for (var i = 0, max = all.length; i < max; i++) {
      var elem = all[i];
      elem.setAttribute("data-phantom-terminal-id", i);
    }

    var inputs = document.getElementsByTagName("input");

    for (var i = 0, max = inputs.length; i < max; i++) {
      var elem = inputs[i];
      elem.setAttribute("data-phantom-value", elem.value);
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
    $(elem).attr("value", $(elem).attr("data-phantom-value"));
  });
}

async function renderHtml(browser) {
  await enumeratePageElements(browser);
  const content = await browser.getHTML("html");
  const $ = cheerio.load(content);
  filter($);
  decorate($);
  fs.writeFileSync("browsed.html", $.html());
}
