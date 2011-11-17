<?php
// Copyright 2004-present Facebook. All Rights Reserved.

/**
 * Deactivates all of a user's affiliations: for use when a user deactivates.
 * This function assumes that $userid was active and is becoming deactivated.
 *
 * @param   int $userid
 * @author  moskov
 */
function deactivate_affiliations($userid) {
  $conn_w = id_get_conn($userid, 'w');
  // unlink active affiliations
  $sql = 'SELECT `network_key` FROM `affiliations_data` ' .
         'WHERE `id` = %d AND `status` != %d AND `confirmed` = %d';
  $ret = queryf(
    $conn_w,
    $sql,
    $userid,
    AffilNetworkStatusConst::REMOVED,
    AffilMiscConst::NETWORK_CONFIRMED_YES
    );
  while ($row = mysql_fetch_array($ret)) {
    list($network_key) = $row;
    remove_affiliation_link($userid, $network_key, $is_deactivate = true);
  }
  // mark id deactivated in affilitaions_data
  //    (wisdom etc needs this data duplication from info.confirmed)

  $sql = 'UPDATE `affiliations_data` SET `id_is_active` = %d, ' .
         '`row_updated_time` = UNIX_TIMESTAMP() WHERE `id` = %d';
  queryf($conn_w, $sql, false, $userid);
}

// test



