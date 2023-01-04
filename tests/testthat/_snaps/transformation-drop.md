# DropTransformation shiny ui

    Code
      DropTransformation$shiny$ui("test")
    Output
      <div class="row">
        <div class="col-sm-4">
          <div class="form-group shiny-input-container">
            <label class="control-label" for="test-drop_cols">Columns</label>
            <select id="test-drop_cols" class="selectpicker form-control" multiple="multiple"></select>
          </div>
        </div>
        <div class="col-sm-2">
          <div class="form-group shiny-input-container">
            <label class="control-label" id="test-drop_name-label" for="test-drop_name">New name</label>
            <input id="test-drop_name" type="text" class="form-control" value="df"/>
          </div>
        </div>
      </div>

