# SelectTransformation shiny ui

    Code
      SelectTransformation$shiny$ui("test")
    Output
      <div class="row">
        <div class="col-sm-4">
          <div class="form-group shiny-input-container">
            <label class="control-label" for="test-select_cols">Columns</label>
            <select id="test-select_cols" class="selectpicker form-control" multiple="multiple"></select>
          </div>
        </div>
        <div class="col-sm-2">
          <div class="form-group shiny-input-container">
            <label class="control-label" id="test-select_name-label" for="test-select_name">New name</label>
            <input id="test-select_name" type="text" class="form-control" value="df"/>
          </div>
        </div>
      </div>

