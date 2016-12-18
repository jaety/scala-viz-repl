var Components = {
  "Text" : function (container, componentState) {
    container.getElement().html("<h2>" + componentState.text + "</h2>");
  },
  "LineChart": function (container, componentState) {
    Plotly.newPlot(container.getElement()[0], componentState.traces);
  }
};
