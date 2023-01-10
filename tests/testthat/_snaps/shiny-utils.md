# input_with_checkbox works

    Code
      input_with_checkbox(textInput("x", "x", "x"), "chx", "lbl")
    Output
      <div class="form-group shiny-input-container">
        <label class="control-label" id="x-label" for="x">x</label>
        <div class="input-group">
          <label class="input-group-addon">
            <input id="chx" type="checkbox" style="vertical-align: middle"/>
            <span style="vertical-align: middle">lbl</span>
          </label>
          <input id="x" type="text" class="form-control" value="x"/>
        </div>
      </div>

---

    Code
      input_with_checkbox(textInput("x", "x", "x"), "chx", "lbl", checkboxValue = TRUE)
    Output
      <div class="form-group shiny-input-container">
        <label class="control-label" id="x-label" for="x">x</label>
        <div class="input-group">
          <label class="input-group-addon">
            <input id="chx" checked="checked" type="checkbox" style="vertical-align: middle"/>
            <span style="vertical-align: middle">lbl</span>
          </label>
          <input id="x" type="text" class="form-control" value="x"/>
        </div>
      </div>

---

    Code
      input_with_checkbox(textInput("x", "x", "x"), "chx", "lbl", side = "right")
    Output
      <div class="form-group shiny-input-container">
        <label class="control-label" id="x-label" for="x">x</label>
        <div class="input-group">
          <input id="x" type="text" class="form-control" value="x"/>
          <label class="input-group-addon">
            <input id="chx" type="checkbox" style="vertical-align: middle"/>
            <span style="vertical-align: middle">lbl</span>
          </label>
        </div>
      </div>

