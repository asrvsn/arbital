import React, { Component, PropTypes } from 'react'

import {Card, CardActions, CardHeader, CardMedia, CardTitle, CardText} from 'material-ui/Card';
import {List} from 'material-ui/List';
import Toggle from 'material-ui/Toggle';

import AuthoredListItem from './AuthoredListItem';
import GetterHOC from '../hoc/GetterHOC';

const styles = {
  block: {
    maxWidth: 250,
  },
  toggle: {
    marginBottom: 16,
  },
  thumbOff: {
    backgroundColor: '#ffcccc',
  },
  trackOff: {
    backgroundColor: '#ff9d9d',
  },
  thumbSwitched: {
    backgroundColor: 'red',
  },
  trackSwitched: {
    backgroundColor: '#ff9d9d',
  },
  labelStyle: {
    color: 'red',
  },
}

class Claim extends Component {

  constructor(props) {
    super(props)
    this.state = {
      showingFor: true
    }
  }

  render() {
    const { claim, items } = this.props
    const { showingFor } = this.state

    const shownItems = items.filter(item => item.isFor == showingFor)

    return (
      <Card>
        <CardTitle title={claim.text} subtitle={claim.author} />
        <Toggle
          label={forAgainst}
          thumbStyle={styles.thumbOff}
          trackStyle={styles.trackOff}
          thumbSwitchedStyle={styles.thumbSwitched}
          trackSwitchedStyle={styles.trackSwitched}
          labelStyle={styles.labelStyle}
          onToggle={(e, i) => this.toggleForAgainst()}
        />
        <CardText>
          <List>
            { shownItems.map(item =>
                <AuthoredListItem
                  text={item.argumentText}
                  authorId={item.argumentAuthorId}
                  authorName={item.argumentAuthorName}
                  hrefPath={'/arguments/' + item.argumentId}
                />
              )
            }
          </List>
        </CardText>
      </Card>
    )
  }

  toggleForAgainst() {
    this.setState({
      showingFor: ! this.state.showingFor
    })
  }
}

export default GetterHOC(
  Claim,
  (props) => ([
    {
      path: props.location.pathName,
      mapResponseToProps: (resp) => {claim: JSON.parse(resp.body)}
    },
    {
      path: props.location.pathName + '/items',
      mapResponseToProps: (resp) => {items: JSON.parse(resp.body)}
    }
  ])
)
