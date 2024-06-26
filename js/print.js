const puppeteer = require("puppeteer");
const path = require("path");

(async () => {
  const filename = process.argv[2];
  const outputfilename = process.argv[3];

  if (!filename) {
    console.error("Please provide a filename as a command line argument.");
    process.exit(1);
  }

  if (!outputfilename) {
    console.error("Please provide a second filename for output as a command line argument.");
    process.exit(1);
  }

  const browser = await puppeteer.launch({
    args: ["--no-sandbox", "--disable-setuid-sandbox"],
  });
  const page = await browser.newPage();
  const filePath = path.resolve(__dirname, filename);
  await page.goto(`file://${filePath}`, { waitUntil: "networkidle2" });

  await page.pdf({
    path: outputfilename,
    format: "A4",
    printBackground: true,
    margin: {
      top: "2cm",
      right: "2cm",
      bottom: "2cm",
      left: "2cm",
    },
    displayHeaderFooter: true,
    headerTemplate: "<span></span>",
    footerTemplate: `
      <div style="width: 100%; text-align: center; font-size: 10px; color: #333;">
        <span class="pageNumber"></span> / <span class="totalPages"></span>
      </div>
    `,
  });

  await browser.close();
})();
