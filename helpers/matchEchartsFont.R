
matchEchartsfontJS <- tags$head(tags$script(HTML('(() => {

  if (!window.echarts) {
    console.warn("ECharts not found on page.");
    return;
  }
  
  // Get Bootstrap/bslib body font
  const getFont = () => {
    const cssVar = getComputedStyle(document.documentElement)
    .getPropertyValue("--bs-body-font-family").trim();
    const body = getComputedStyle(document.body).fontFamily;
    return cssVar || body || "system-ui, sans-serif";
  };
  
  const font = getFont();
  console.log("Applying font:", font);
  
  // Apply to *every* chart currently on the page
  document.querySelectorAll(".echarts4r, .echarts, .html-widget").forEach(el => {
    const chart = echarts.getInstanceByDom(el);
    if (!chart) return;
    
    chart.setOption({
      textStyle: { fontFamily: font },
      title: { textStyle: { fontFamily: font } },
      legend: { textStyle: { fontFamily: font } },
      tooltip: { textStyle: { fontFamily: font } },
      xAxis: { axisLabel: { fontFamily: font }, nameTextStyle: { fontFamily: font } },
      yAxis: { axisLabel: { fontFamily: font }, nameTextStyle: { fontFamily: font } }
    }, false);
    
    chart.resize(); // force redraw
  });
  
  console.log("✅ Bootstrap font applied to all ECharts instances");
})();'

)))