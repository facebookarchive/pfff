<?php

function foo() {

  $albums = null;
  if ($media_type_mask & MediaType::PHOTO) {

    $albums = new AssociatedAlbumSelector(
      $this->getViewerContext(),
      $thishis->getID(),
      $this->getMediaSource($media_type_mask)->getAssoc('album'),
      $this->countableAlbums,
      array(Ent::load('ApproximateCount'))
      );
  }
}

class Foo {
  public function prepareLinkedObjects() {
    $object_type = yield wait_for($ent->genObjectType());
    $config = yield wait_for($object_type->genPropertyConfig());

    $urls = array();
    foreach ($config->getFlatProperties(array(OGPropertyConst::TYPE_REFERENCE)) as $property) {
      $value = $ent->getPropertyForSelector(
        $property->getFullyQualifiedNameXxx()
      );
      if (is_array($value)) {
        $urls = array_merge($urls, $value);
      } else {
        $urls[] = $value;
      }
    }
    $urls = array_filter($urls);
    yield wait_for_result($ent->genAssociatedObjects('all-urls', $urls));
  }
}



