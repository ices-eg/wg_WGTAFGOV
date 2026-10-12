// Draws the charts on the status pages from the data written by
// _scripts/status.R (the JSON block with id "taf-status-data").
(function () {
  const source = document.getElementById("taf-status-data");
  if (!source || !window.Plotly) return;
  const data = JSON.parse(source.textContent);

  const blue = "#2a78d6";
  const orange = "#eb6834";
  const ink = "#52514e";
  const grid = "#e6e5e0";
  const config = { displayModeBar: false, responsive: true };

  function layout(extra) {
    return Object.assign(
      {
        margin: { t: 10, r: 10, b: 40, l: 50 },
        font: { family: "inherit", size: 13, color: ink },
        paper_bgcolor: "rgba(0,0,0,0)",
        plot_bgcolor: "rgba(0,0,0,0)",
        legend: { orientation: "h", y: 1.12, x: 0 },
        bargap: 0.3,
        bargroupgap: 0.06,
        hoverlabel: { font: { family: "inherit" } },
      },
      extra
    );
  }

  const yaxis = (title, more) =>
    Object.assign({ title: { text: title }, gridcolor: grid, zeroline: false, rangemode: "tozero" }, more);

  // ---- all repositories ----------------------------------------------------

  if (document.getElementById("taf-repos-by-year")) {
    const d = data.repos_by_year;
    Plotly.newPlot(
      "taf-repos-by-year",
      [{
        type: "bar",
        x: d.map((r) => r.year),
        y: d.map((r) => r.n),
        marker: { color: blue },
        hovertemplate: "%{x}: %{y} repositories<extra></extra>",
      }],
      layout({ xaxis: { dtick: 1 }, yaxis: yaxis("Repositories") }),
      config
    );
  }

  if (document.getElementById("taf-repos-by-type")) {
    const d = data.repos_by_type.slice().reverse();
    Plotly.newPlot(
      "taf-repos-by-type",
      [{
        type: "bar",
        orientation: "h",
        x: d.map((r) => r.n),
        y: d.map((r) => r.type),
        marker: { color: blue },
        hovertemplate: "%{y}: %{x} repositories<extra></extra>",
      }],
      layout({
        margin: { t: 10, r: 10, b: 40, l: 170 },
        xaxis: { title: { text: "Repositories" }, gridcolor: grid, zeroline: false },
        yaxis: { ticksuffix: "  " },
      }),
      config
    );
  }

  // ---- stock assessments ---------------------------------------------------

  const stocks = data.stocks_by_year;
  const categories = ["Category 1", "Categories 2-6"];
  // years since the first with an assessment implemented in TAF
  const firstYear = Math.min(...stocks.filter((r) => r.taf > 0).map((r) => r.year));
  const years = [...new Set(stocks.map((r) => r.year))].filter((y) => y >= firstYear).sort();

  const select = document.getElementById("taf-year");
  if (select && years.length) {
    // as in the dashboard: show last year's assessments until August
    const now = new Date();
    let start = now.getFullYear() - (now.getMonth() < 7 ? 1 : 0);
    if (!years.includes(start)) start = years[years.length - 1];

    years.slice().reverse().forEach((y) => {
      const option = new Option(y, y, y === start, y === start);
      select.add(option);
    });

    const draw = () => {
      const year = Number(select.value);
      const rows = categories.map((c) => stocks.find((r) => r.year === year && r.category === c) || { stocks: 0, taf: 0 });
      Plotly.react(
        "taf-stocks",
        [
          {
            type: "bar",
            name: "Stocks",
            x: categories,
            y: rows.map((r) => r.stocks),
            marker: { color: blue },
            hovertemplate: "%{x}: %{y} stocks<extra></extra>",
          },
          {
            type: "bar",
            name: "Stocks implemented in TAF",
            x: categories,
            y: rows.map((r) => r.taf),
            marker: { color: orange },
            hovertemplate: "%{x}: %{y} implemented in TAF<extra></extra>",
          },
        ],
        layout({ barmode: "group", yaxis: yaxis("Count") }),
        config
      );

      const egs = data.stocks_by_eg.filter((r) => r.year === year).sort((a, b) => b.percent - a.percent);
      document.querySelector("#taf-eg tbody").innerHTML = egs
        .map((r) => `<tr><td>${r.eg}</td><td class="num">${r.n}</td><td class="num">${r.percent.toFixed(1)}%</td></tr>`)
        .join("");
      document.querySelectorAll(".taf-year-label").forEach((el) => (el.textContent = year));
    };
    select.addEventListener("change", draw);
    draw();
  }

  if (document.getElementById("taf-stocks-trend") && years.length) {
    const colors = [blue, orange];
    const traces = categories.map((c, i) => {
      const rows = years.map((y) => stocks.find((r) => r.year === y && r.category === c));
      return {
        type: "scatter",
        mode: "lines+markers",
        name: c,
        x: years,
        y: rows.map((r) => (r ? (100 * r.taf) / r.stocks : null)),
        customdata: rows.map((r) => (r ? [r.taf, r.stocks] : [0, 0])),
        line: { color: colors[i], width: 2 },
        marker: { color: colors[i], size: 8 },
        hovertemplate: c + ", %{x}: %{y:.1f}% (%{customdata[0]} of %{customdata[1]} stocks)<extra></extra>",
      };
    });
    const max = Math.max(...traces.flatMap((t) => t.y));
    Plotly.newPlot(
      "taf-stocks-trend",
      traces,
      layout({
        xaxis: { dtick: 1, range: [years[0] - 0.3, years[years.length - 1] + 0.3] },
        yaxis: yaxis("Percentage of stocks", { ticksuffix: "%", range: [0, Math.ceil(max / 10) * 10 + 2] }),
      }),
      config
    );
  }
})();
