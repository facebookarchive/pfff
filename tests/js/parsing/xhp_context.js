var MAdsCreate = {};
MAdsCreate.renderPage = function(data, node) {
  init();
  React.renderComponent(
    <MAdsPage
      scrollContents={[
         <MAdsDestinationsView destinations={data.destinations} />
      ]}
    />,
    node
  );
};

$o = {
  render: function() {
    var projectOptions = [
       <option value={DEFAULT_VALUE} key={DEFAULT_VALUE}>
        ---
      </option>
    ];
  }
};

o = {
       userAddForm: this.state.isDialogOpen && <XUIDialog>xxx</XUIDialog>
    };

var testCases = [
       [<InputText value="hello" />, 'hello']
                 ];
