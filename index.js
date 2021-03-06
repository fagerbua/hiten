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
    answer = readlineSync.question("> ");
    console.log("Handling", answer);
    try {
      if (answer[0] === "C") {
        await browser.click('[data-hiten-id="' + answer.slice(1) + '"]');
      }
      if (answer[0] === "I") {
        const inputData = answer.slice(answer.indexOf("=") + 1);
        const selector =
          '[data-hiten-id="' + answer.slice(1, answer.indexOf("=")) + '"]';
        await browser.click(selector);
        await browser.setValue(selector, inputData);
      }
      if (answer[0] === "R") {
        await renderHtml(browser);
      }
      if (answer[0] === "O") {
        const url = answer.slice(1);
        await browser.url(url);
      }
    } catch (e) {
      console.warn("Error:", e);
    }
  }
})();

async function enumeratePageElements(browser) {
  await browser.execute(function() {
    var isInvisible = function(elem) {
      var displayStyle = window.getComputedStyle(elem);
      return (
        (displayStyle.display && displayStyle.display === "none") ||
        elem.getAttribute("hidden")
      );
    };

    var all = document.getElementsByTagName("*");

    for (var i = 0, max = all.length; i < max; i++) {
      var elem = all[i];
      if (isInvisible(elem)) {
        elem.setAttribute("data-hiten-hidden", true);
      }
      elem.setAttribute("data-hiten-id", i);
    }

    var inputs = document.getElementsByTagName("input");

    for (var i = 0, max = inputs.length; i < max; i++) {
      var elem = inputs[i];
      elem.setAttribute("data-hiten-value", elem.value);
    }
  });
}

async function pageLoadState(browser) {
  return (await browser.execute(function() {
    return document.readyState;
  })).value;
}

function filter($) {
  const scriptStuff = $("script, noscript");
  scriptStuff.remove();
}

function decorate($) {
  decorateClickableElements($);
  decorateTextInputs($);
  decorateHeadings($);
  removeInvisibleElements($);
}

function decorateClickableElements($) {
  const clickables = $("a, button");
  clickables.each((i, clickable) => {
    const elem = $(clickable);
    elem.html(`[[${elem.attr("data-hiten-id")}:C:${elem.html()}]]`);
  });
}

function decorateHeadings($) {
  const headings = $("h1, h2, h3, h4, h5, h6");
  headings.each((i, heading) => {
    const elem = $(heading);
    const level = parseInt(elem[0].name[1]);
    const prefix = "*".repeat(level);
    elem.html(`${prefix} ${elem.html()}`);
  });
}

function decorateTextInputs($) {
  const textInputs = $("input[type=text], input[type=textarea]");
  textInputs.each((i, elem) => {
    $(elem).prepend("[[I" + $(elem).attr("data-hiten-id") + "]]");
    $(elem).attr("value", $(elem).attr("data-phantom-value"));
  });
}

function removeInvisibleElements($) {
  $("*[data-hiten-hidden=true]").remove();
}

async function renderHtml(browser) {
  let loadState = await pageLoadState(browser);
  while (loadState !== "interactive" && loadState !== "complete") {
    await new Promise(resolve => setTimeout(resolve, 1000));
    loadState = await pageLoadState(browser);
  }
  await enumeratePageElements(browser);
  const content = await browser.getHTML("html");
  const $ = cheerio.load(content);
  filter($);
  decorate($);
  const fileName = "browsed.html";
  fs.writeFileSync(fileName, $.html());
  console.log("WROTE", fileName);
}
