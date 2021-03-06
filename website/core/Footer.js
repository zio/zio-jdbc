/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import Gitter from 'react-sidecar';

const React = require('react');

class Footer extends React.Component {
  docUrl(doc, language) {
    const baseUrl = this.props.config.baseUrl;
    const docsUrl = this.props.config.docsUrl;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ''}`;
    const langPart = `${language ? `${language}/` : ''}`;
    return `${baseUrl}${docsPart}${langPart}${doc}`;
  }

  pageUrl(doc, language) {
    const baseUrl = this.props.config.baseUrl;
    return baseUrl + (language ? `${language}/` : '') + doc;
  }

  render() {
    return (
      <footer className="nav-footer" id="footer">
        <section className="sitemap">
          <a href={this.props.config.baseUrl} className="nav-home">
            {this.props.config.footerIcon && (
              <img
                src={this.props.config.baseUrl + this.props.config.footerIcon}
                alt={this.props.config.title}
              />
            )}
          </a>
          <div>
            <h5>GitHub</h5>
            <a
              className="github-button"
              href="https://github.com/zio/zio-jdbc"
              data-icon="octicon-star"
              data-count-href="/zio/zio-jdbc/stargazers"
              data-show-count="true"
              data-count-aria-label="# stargazers on GitHub"
              aria-label="Star this project on GitHub">
              Star
            </a>
          </div>
          <div>
              <h5>Chat with us on Discord</h5>
              <a href="https://discord.gg/2ccFBr4">
                <img src={this.props.config.baseUrl + "img/discord.png"} width="120" alt="discord"/>
              </a>
          </div>
          <div>
            <h5>Additional resources</h5>
            <a
                href="https://zio.dev">
              ZIO Homepage
            </a>
            <a
              href="/zio-jdbc/api">
              Scaladoc
            </a>
          </div>
        </section>
        <section className="copyright">{this.props.config.copyright}</section>
      </footer>
    );
  }
}

module.exports = Footer;
