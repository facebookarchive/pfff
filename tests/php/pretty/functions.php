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

/**
 * removes an affiliation for a user or object in three places: the home db
 * for the id, and the home db for the network, the groups network editor
 *
 * @param int   the user or object in question
 * @param int   the network they are becoming un-affiliated with
 * @param bool  if this is an account deactivation don't disable the
 *              network assocs, they will get disabled on their own
 */
function remove_affiliation_link(
  $userid,
  $network_key,
  $is_deactivate = false) {
  // 1. remove it from the user's db
  $conn_id_w = id_get_conn($userid, 'w');
  $conn_network_w = network_get_conn($network_key, 'w');
  $sql_remove = 'DELETE FROM affiliations WHERE id = %d AND network_key = %d';
  $cachekeys = affiliation_get_cachekeys_for_add_remove($userid, $network_key);
  queryf_memcache($conn_id_w, $cachekeys, $sql_remove, $userid, $network_key);

  // 2. remove it from the network's db
  //    unnecessary if a user's home db is the same as the network's
  if (id_to_db($userid) != network_to_db($network_key)) {
    queryf($conn_network_w, $sql_remove, $userid, $network_key);
  }
  hook_remove_affiliation($userid, $network_key);

  // 3. remove a group association as we transition over to groups
  // for deactivations we don't want to delete the group assocs as these
  // will be handled by the assoc deactivation code
  if (fbid_in_uid_range($userid) && !$is_deactivate) {
    if ($fbid = network_get_fbid($network_key)) {
      $viewer_context = ViewerContext::newAllPowerfulViewerContext();
      $profile = fbobj_get_profile($fbid);
      if (group_is_managed_tribe($fbid, $profile)) {
        id(new EntTribeEditor($viewer_context, $fbid))->removeMember($userid);
      } else if (group_is_network($fbid, $profile)) {
        id(new EntNetworkEditor($viewer_context, $network_key, $fbid))
        ->removeMember($userid);
      } else {
        FBLogger('networks')->mustfix(
          'Trying to add member to network with wrong version type'
        );
      }

    } else if (extract_network_type($network_key) != $GLOBALS['TYPE_GEO']) {
      FBLogger('networks')->mustfix(
        'Failed to retrieve network FBID for key: %s',
        $network_key
      );
    }
  }
}





































