package com.mango.photoalbum.service;

import com.mango.photoalbum.model.PhotoAlbum;
import com.mango.photoalbum.model.PhotoAlbumCo;

import java.util.List;

public interface PhotoAlbumService {

    PhotoAlbum save(PhotoAlbumCo PhotoAlbumCo);

    void delete(String albumId);

    PhotoAlbum get(String albumId);

    List<PhotoAlbum> list();
}
