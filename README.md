hstorrent [![Build Status](https://travis-ci.org/hstorrent/hstorrent.svg?branch=master)](https://travis-ci.org/hstorrent/hstorrent)
=========

BitTorrent library in Haskell. It has few main goals:

* Efficient and scalable – it needs to be able to handle tens of
  thousands of announcing torrents without insane hardware
  requirements. The performance should be comparable to at least
  libtorrent.

* Resillent – it can not crash when things ‘go wrong’. Most existing
  libraries fail here.

* Support for all the common extensions – everything except perhaps
  BEP 8.

* Sane API – it is intended to be a library that others can build upon
  rather than a component for a specific client.

## BEPs

Below is a table of BEPs and the status of their support.

<table border="1">
<tr>
<th>Number</th>
<th>Description</th>
<th>Status</th>
</tr>

<tr><td>0</td> <td>Index of BitTorrent Enhancement Proporsals</td> <td>✗</td></tr>
<tr><td>1</td> <td>The BEP Process</td> <td>✗</td></tr>
<tr><td>2</td> <td>Sample reStructured Text BEP Template</td> <td>✗</td></tr>
<tr><td>3</td> <td>The BitTorrent Protocol Specification</td> <td>✗</td></tr>
<tr><td>4</td> <td>Known Number Allocations</td> <td>✗</td></tr>
<tr><td>20</td> <td>Peer ID Conventions</td> <td>✗</td></tr>
<tr><td>1000</td> <td>Pending Standards Track Documents</td> <td>✗</td></tr>
<tr><td>9</td> <td>Extension for Peers to Send Metadata Files</td> <td>✗</td></tr>
<tr><td>23</td> <td>Tracker Returns Compact Peer Lists</td> <td>✗</td></tr>
<tr><td>5</td> <td>DHT Protocol</td> <td>✗</td></tr>
<tr><td>6</td> <td>Fast Extension</td> <td>✗</td></tr>
<tr><td>7</td> <td>IPv6 Tracker Extension</td> <td>✗</td></tr>
<tr><td>10</td> <td>Extension Protocol</td> <td>✗</td></tr>
<tr><td>12</td> <td>Multitracker Metadata Extension</td> <td>✗</td></tr>
<tr><td>15</td> <td>UDP Tracker Protocol</td> <td>✗</td></tr>
<tr><td>16</td> <td>Superseeding</td> <td>✗</td></tr>
<tr><td>17</td> <td>HTTP Seeding (Hoffman-style)</td> <td>✗</td></tr>
<tr><td>18</td> <td>Search Engine Specification</td> <td>✗</td></tr>
<tr><td>19</td> <td>HTTP/FTP Seeding (GetRight-style)</td> <td>✗</td></tr>
<tr><td>21</td> <td>Extension for Partial Seeds</td> <td>✗</td></tr>
<tr><td>22</td> <td>BitTorrent Local Tracker Discovery Protocol</td> <td>✗</td></tr>
<tr><td>24</td> <td>Tracker Returns External IP</td> <td>✗</td></tr>
<tr><td>26</td> <td>Zeroconf Peer Advertising and Discovery</td> <td>✗</td></tr>
<tr><td>27</td> <td>Private Torrents</td> <td>✗</td></tr>
<tr><td>28</td> <td>Tracker exchange</td> <td>✗</td></tr>
<tr><td>29</td> <td>uTorrent transport protocol</td> <td>✗</td></tr>
<tr><td>30</td> <td>Merkle tree torrent extension</td> <td>✗</td></tr>
<tr><td>31</td> <td>Tracker Failure Retry Extension</td> <td>✗</td></tr>
<tr><td>32</td> <td>IPv6 extension for DHT</td> <td>✗</td></tr>
<tr><td>33</td> <td>DHT scrape</td> <td>✗</td></tr>
<tr><td>34</td> <td>DNS Tracker Preferences</td> <td>✗</td></tr>
<tr><td>35</td> <td>Torrent Signing</td> <td>✗</td></tr>
<tr><td>36</td> <td>Torrent RSS feeds</td> <td>✗</td></tr>
<tr><td>38</td> <td>Finding Local Data Via Torrent File Hints</td> <td>✗</td></tr>
<tr><td>39</td> <td>Updating Torrents Via Feed URL</td> <td>✗</td></tr>
<tr><td>40</td> <td>Canonical Peer Priority</td> <td>✗</td></tr>
<tr><td>41</td> <td>UDP Tracker Protocol Extensions</td> <td>✗</td></tr>
<tr><td>42</td> <td>DHT Security Extension</td> <td>✗</td></tr>
<tr><td>8</td> <td>Tracker Peer Obfuscation</td> <td>?</td></tr>

</table>

✓ = Done <br />
~ = Partial/In progress <br />
? = Possible future feature <br />
✗ = Not started/Completely broken <br />

## Committing

Please document everything you write, including module headers. Please
add tests to what you write.

80 columns, two spaces, no trailing whitespace.

UnicodeSyntax OK if you like it.

Developed against 7.8.3, all extensions are free game.

Please add yourself to AUTHORS!

## Reading material

* [List of BEPs][beplist]

* [Combinatorrent, a Haskell torrent client][combinatorrent]

* [bittorrent Haskell library + sister repos][cobit]

* [conjure, a client using STM][conjure]

* [Various bencoding libraries][bencoders]

* [BitTorrent specification (Theory.org)][twbittorrent]

* [BitTorrent tracker protocol][twtracker]

* [BitTorrent tracker written in Haskell][iterateetracker]

[beplist]: http://www.bittorrent.org/beps/bep_0000.html
[combinatorrent]: https://github.com/jlouis/combinatorrent
[cobit]: https://github.com/cobit
[conjure]: http://hackage.haskell.org/package/conjure
[bencoders]: http://hackage.haskell.org/packages/search?terms=bencode
[twbittorrent]: https://wiki.theory.org/BitTorrentSpecification
[twtracker]: https://wiki.theory.org/BitTorrent_Tracker_Protocol
[iterateetracker]: https://github.com/iteratee/haskell-tracker
