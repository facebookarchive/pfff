module Common {

function last_week() {
  now = Date.now();
  Date.advance_by_days(now, -7);
}

}
