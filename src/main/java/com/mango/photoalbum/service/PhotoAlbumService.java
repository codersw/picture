package com.mango.photoalbum.service;

import com.mango.photoalbum.model.*;

import java.text.ParseException;
import java.util.List;

public interface PhotoAlbumService {

    PhotoAlbum save(PhotoAlbumCo PhotoAlbumCo);

    PhotoAlbum saveV1(PhotoAlbumV1Co photoAlbumV1Co);

    void delete(String albumId);

    PhotoAlbum get(String albumId);

    Integer total(PhotoAlbumListCo photoAlbumListCo);

    List<PhotoAlbum> list(PhotoAlbumListCo photoAlbumListCo);

    Integer totalV1(PhotoAlbumListV1Co photoAlbumListV1Co);

    List<PhotoAlbum> listV1(PhotoAlbumListV1Co photoAlbumListV1Co);

    Integer totalAdmin(PhotoAlbumListV1Co photoAlbumListV1Co);

    List<PhotoAlbum> listAdmin(PhotoAlbumListV1Co photoAlbumListV1Co);
}
